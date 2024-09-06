module Main where

import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Version (showVersion)
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Options.Applicative hiding (action)
import System.Exit

import GetTested.CLI.Types
import GetTested.Extract
import GetTested.Types
import Paths_get_tested (version)

main :: IO ()
main = do
  result <- execParser (parseOptions `withInfo` "Generate a test matrix from the tested-with stanza of your cabal file")
  processingResult <- runCLIEff $ runOptions result
  case processingResult of
    Right json -> putStrLn $ ByteString.unpack json
    Left (CabalFileNotFound path) -> do
      putStrLn $ "get-tested: Could not find cabal file at path " <> path
      exitFailure
    Left (CabalFileCouldNotBeParsed path) -> do
      putStrLn $ "get-tested: Could not parse cabal file at path " <> path
      exitFailure
    Left (NoCompilerVersionsFound path) -> do
      putStrLn $ "get-tested: No compilers found in" <> path
      exitFailure
    Left (IncompatibleOptions opt1 opt2) -> do
      putStrLn $ Text.unpack $ "get-tested: Incompatible options: " <> opt1 <> " and " <> opt2 <> " cannot be passed simultaneously."
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
    <*> switch (long "newest" <> help "Enable only the newest GHC version found in the cabal file")
    <*> switch (long "oldest" <> help "Enable only the oldest GHC version found in the cabal file")
      <**> simpleVersioner (showVersion version)

runOptions :: Options -> Eff [Console, FileSystem, Error ProcessingError, IOE] ByteString
runOptions options = do
  checkIncompatibleRelativeOptions options
  genericPackageDescription <- loadFile options.path
  selectedCompilers <-
    filterCompilers options
      <$> extractTestedWith options.path genericPackageDescription
  let filteredList =
        processOSFlag MacOS options.macosFlag options.macosVersion
          <> processOSFlag Ubuntu options.ubuntuFlag options.ubuntuVersion
          <> processOSFlag Windows options.windowsFlag options.windowsVersion
  if null filteredList
    then pure $ Aeson.encode selectedCompilers
    else do
      let include = PlatformAndVersion <$> filteredList <*> selectedCompilers
      pure $ "matrix=" <> Aeson.encode (ActionMatrix include)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

processOSFlag
  :: RunnerOS
  -- ^ OS flag we're processing
  -> Bool
  -- ^ legacy fallback
  -> Maybe Text
  -- ^ explicit version
  -> Vector Text
processOSFlag runnerOS legacyFallback mExplicitVersion =
  case mExplicitVersion of
    Just explicitVersion -> Vector.singleton (display runnerOS <> "-" <> explicitVersion)
    Nothing ->
      if legacyFallback
        then Vector.singleton $ display runnerOS <> "-latest"
        else Vector.empty

checkIncompatibleRelativeOptions
  :: (Error ProcessingError :> es)
  => Options
  -> Eff es ()
checkIncompatibleRelativeOptions options = do
  when (options.newest && options.oldest) $
    throwError $
      IncompatibleOptions
        "--newest"
        "--oldest"

runCLIEff
  :: Eff [Console, FileSystem, Error ProcessingError, IOE] a
  -> IO (Either ProcessingError a)
runCLIEff action = do
  action
    & Console.runConsole
    & FileSystem.runFileSystem
    & Error.runErrorNoCallStack
    & runEff
