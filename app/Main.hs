module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Options.Applicative
import System.Exit

import Data.Version (showVersion)
import Extract
import Paths_get_tested (version)
import Types

data Options = Options
  { path :: FilePath
  , macosFlag :: Bool
  , macosVersion :: Maybe Text
  , ubuntuFlag :: Bool
  , ubuntuVersion :: Maybe Text
  , windowsFlag :: Bool
  , windowsVersion :: Maybe Text
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  result <- execParser (parseOptions `withInfo` "Generate a test matrix from the tested-with stanza of your cabal file")
  processingResult <- runEff . runErrorNoCallStack $ runOptions result
  case processingResult of
    Right json -> ByteString.putStrLn json
    Left (CabalFileNotFound path) -> do
      putStrLn $ "Could not find cabal file at path " <> path
      exitFailure
    Left (CabalFileCouldNotBeParsed path) -> do
      putStrLn $ "Could not parse cabal file at path " <> path
      exitFailure

parseOptions :: Parser Options
parseOptions =
  Options
    <$> argument str (metavar "FILE")
    <*> switch (long "macos" <> help "(legacy) Enable the macOS runner's latest version")
    <*> optional (strOption (long "macos-version" <> metavar "VERSION" <> help "Enable the macOS runner with the selected version"))
    <*> switch (long "ubuntu" <> help "(legacy) Enable the Ubuntu runner's latest version")
    <*> optional (strOption (long "ubuntu-version" <> metavar "VERSION" <> help "Enable the Ubuntu runner with the selected version"))
    <*> switch (long "windows" <> help "(legacy) Enable the Windows runner's latest version")
    <*> optional (strOption (long "windows-version" <> metavar "VERSION" <> help "Enable the Windows runner with the selected version"))
      <**> simpleVersioner (showVersion version)

runOptions :: Options -> Eff [Error ProcessingError, IOE] ByteString
runOptions options = do
  genericPackageDescription <- loadFile options.path
  let supportedCompilers = extractTestedWith genericPackageDescription
      filteredList =
        processFlag MacOS options.macosFlag options.macosVersion
          <> processFlag Ubuntu options.ubuntuFlag options.ubuntuVersion
          <> processFlag Windows options.windowsFlag options.windowsVersion
  pure $
    if null filteredList
      then Aeson.encode supportedCompilers
      else do
        let include = PlatformAndVersion <$> filteredList <*> supportedCompilers
        "matrix=" <> Aeson.encode (ActionMatrix include)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

processFlag
  :: RunnerOS
  -- ^ OS flag we're processing
  -> Bool
  -- ^ explicit version
  -> Maybe Text
  -- ^ legacy fallback
  -> Vector Text
processFlag runnerOS legacyFallback mExplicitVersion =
  case mExplicitVersion of
    Just explicitVersion -> Vector.singleton (display runnerOS <> "-" <> explicitVersion)
    Nothing ->
      if legacyFallback
        then Vector.singleton $ display runnerOS <> "-latest"
        else Vector.empty
