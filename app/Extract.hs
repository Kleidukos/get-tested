{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Extract
  ( loadFile
  , extractTestedWith
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Fields
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Effectful
import Effectful.Error.Static (Error, throwError)
import System.Directory qualified as System
import System.IO (stdout)

import Types

-- | Loads and parses a Cabal file
loadFile
  :: (Error ProcessingError :> es, IOE :> es)
  => FilePath
  -- ^ The absolute path to the Cabal file
  -> Eff es GenericPackageDescription
loadFile path = do
  exists <- liftIO $ System.doesFileExist path
  unless exists $
    throwError $
      CabalFileNotFound path
  content <- liftIO $ BS.readFile path
  parseString path content

parseString
  :: (Error ProcessingError :> es, IOE :> es)
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

extractTestedWith :: GenericPackageDescription -> Vector Version
extractTestedWith genericPackageDescription =
  Vector.fromList genericPackageDescription.packageDescription.testedWith
    & Vector.filter (\(flavour, _) -> flavour == GHC)
    & Vector.filter (\(_, versionRange) -> isExactVersion versionRange)
    & Vector.map (\(flavour, ThisVersion version) -> (flavour, version))
    & Vector.map snd

isExactVersion :: VersionRange -> Bool
isExactVersion (ThisVersion _) = True
isExactVersion _ = False

logAttention :: (IOE :> es) => Text -> Eff es ()
logAttention message = liftIO $ BS.hPutStrLn stdout $ Text.encodeUtf8 message
