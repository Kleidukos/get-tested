{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GetTested.Extract
  ( loadFile
  , extractTestedWith
  , extractNonEmptyTestedWith
  , filterCompilers
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Function ((&))
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
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.IO.ByteString qualified as FileSystem

import GetTested.CLI.Types
import GetTested.Types

-- | Loads and parses a Cabal file
loadFile
  :: (FileSystem :> es, Error ProcessingError :> es)
  => FilePath
  -- ^ The absolute path to the Cabal file
  -> Eff es GenericPackageDescription
loadFile path = do
  exists <- FileSystem.doesFileExist path
  unless exists $ do
    throwError $ CabalFileNotFound path
  content <- FileSystem.readFile path
  parseString path content

parseString
  :: (Error ProcessingError :> es)
  => String
  -- ^ File name
  -> BS.ByteString
  -> Eff es GenericPackageDescription
parseString name bs = do
  let (_warnings, result) = runParseResult (parseGenericPackageDescription bs)
  case result of
    Right x -> pure x
    Left err -> throwError $ CabalFileCouldNotBeParsed name (show err)

extractTestedWith
  :: GenericPackageDescription
  -> Vector Version
extractTestedWith genericPackageDescription =
  Vector.fromList genericPackageDescription.packageDescription.testedWith
    & Vector.filter (\(flavour, _) -> flavour == GHC)
    & expandUnionVersionRanges
    & Vector.mapMaybe (\(_, versionRange) -> extractThisVersion versionRange)

extractNonEmptyTestedWith
  :: (Error ProcessingError :> es)
  => FilePath
  -> GenericPackageDescription
  -> Eff es (NonEmptyVector Version)
extractNonEmptyTestedWith path genericPackageDescription =
  case NEVector.fromVector (extractTestedWith genericPackageDescription) of
    Nothing -> throwError $ NoCompilerVersionsFound path
    Just compilers -> pure compilers

extractThisVersion :: VersionRange -> Maybe Version
extractThisVersion (ThisVersion version) = Just version
extractThisVersion _ = Nothing

expandUnionVersionRanges
  :: Vector (CompilerFlavor, VersionRange)
  -> Vector (CompilerFlavor, VersionRange)
expandUnionVersionRanges ranges =
  Vector.concatMap
    ( \(f, range) ->
        let expandedVersions = expandUnionVersionRange range
         in Vector.map (f,) expandedVersions
    )
    ranges

expandUnionVersionRange :: VersionRange -> Vector VersionRange
expandUnionVersionRange (ThisVersion v) = Vector.singleton (ThisVersion v)
expandUnionVersionRange (UnionVersionRanges r1 r2) = expandUnionVersionRange r1 <> expandUnionVersionRange r2
expandUnionVersionRange _ = Vector.empty

filterCompilers :: GenerateOptions -> NonEmptyVector Version -> Vector Version
filterCompilers options supportedCompilers
  | options.generateOptionsNewest = Vector.generate 1 $ const $ NEVector.maximum supportedCompilers
  | options.generateOptionsOldest = Vector.generate 1 $ const $ NEVector.minimum supportedCompilers
  | otherwise = NEVector.toVector supportedCompilers
