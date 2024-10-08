{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GetTested.Extract
  ( loadFile
  , extractTestedWith
  , filterCompilers
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Fields
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem

import GetTested.CLI.Types
import GetTested.Types

-- | Loads and parses a Cabal file
loadFile
  :: (Console :> es, FileSystem :> es, Error ProcessingError :> es)
  => FilePath
  -- ^ The absolute path to the Cabal file
  -> Eff es GenericPackageDescription
loadFile path = do
  exists <- FileSystem.doesFileExist path
  unless exists $
    throwError $
      CabalFileNotFound path
  content <- FileSystem.readFile path
  parseString path content

parseString
  :: (Console :> es, Error ProcessingError :> es)
  => String
  -- ^ File name
  -> BS.ByteString
  -> Eff es GenericPackageDescription
parseString name bs = do
  let (_warnings, result) = runParseResult (parseGenericPackageDescription bs)
  case result of
    Right x -> pure x
    Left err -> do
      logAttention (display $ show err)
      throwError $ CabalFileCouldNotBeParsed name

extractTestedWith
  :: (Error ProcessingError :> es)
  => FilePath
  -> GenericPackageDescription
  -> Eff es (NonEmptyVector Version)
extractTestedWith path genericPackageDescription = do
  let exactVersions =
        Vector.fromList genericPackageDescription.packageDescription.testedWith
          & Vector.filter (\(flavour, _) -> flavour == GHC)
          & expandUnionVersionRanges
          & Vector.filter (\(_, versionRange) -> isExactVersion versionRange)
  case NEVector.fromVector exactVersions of
    Nothing ->
      throwError $ NoCompilerVersionsFound path
    Just compilers ->
      pure $
        compilers
          & NEVector.map (\(flavour, ThisVersion version) -> (flavour, version))
          & NEVector.map snd

isExactVersion :: VersionRange -> Bool
isExactVersion (ThisVersion _) = True
isExactVersion _ = False

logAttention :: (Console :> es) => Text -> Eff es ()
logAttention message = Console.putStrLn $ Text.encodeUtf8 message

expandUnionVersionRanges
  :: Vector (CompilerFlavor, VersionRange)
  -> Vector (CompilerFlavor, VersionRange)
expandUnionVersionRanges ranges =
  Vector.concatMap
    ( \(f, range) ->
        let expandedVersions = expandUnionVersionRange range
         in Vector.map (\v -> (f, v)) expandedVersions
    )
    ranges

expandUnionVersionRange :: VersionRange -> Vector VersionRange
expandUnionVersionRange (ThisVersion v) = Vector.singleton (ThisVersion v)
expandUnionVersionRange (UnionVersionRanges r1 r2) = expandUnionVersionRange r1 <> expandUnionVersionRange r2
expandUnionVersionRange _ = Vector.empty

filterCompilers :: Options -> NonEmptyVector Version -> Vector Version
filterCompilers options supportedCompilers
  | options.newest = Vector.generate 1 $ const $ NEVector.maximum supportedCompilers
  | options.oldest = Vector.generate 1 $ const $ NEVector.minimum supportedCompilers
  | otherwise = NEVector.toVector supportedCompilers
