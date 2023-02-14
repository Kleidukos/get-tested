module Extract where

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
import Distribution.Types.VersionRange (VersionRange, withinRange)
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

extractTestedWith :: GenericPackageDescription -> Vector VersionRange
extractTestedWith genericPackageDescription =
  Vector.fromList genericPackageDescription.packageDescription.testedWith
    & Vector.filter (\(flavour, _) -> flavour == GHC)
    & Vector.filter (\(_, versionRange) -> any (`withinRange` versionRange) versionList)
    & Vector.map snd

getVersions :: Vector VersionRange -> Vector Version
getVersions supportedCompilers =
  foldMap
    (\version -> Vector.foldMap (\versionRange -> checkVersion version versionRange) supportedCompilers)
    versionList

checkVersion :: Version -> VersionRange -> Vector Version
checkVersion version versionRange =
  if version `withinRange` versionRange
    then Vector.singleton version
    else Vector.empty

logAttention :: (IOE :> es) => Text -> Eff es ()
logAttention message = liftIO $ BS.hPutStrLn stdout $ Text.encodeUtf8 message
