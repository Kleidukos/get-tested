module Main where

import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Data.Version qualified
import Distribution.Parsec (parsec, runParsecParser)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Console.ByteString.Lazy qualified as Console.Lazy
import Effectful.Error.Static
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Options.Applicative
import System.Exit

import GetTested.CLI.Types
import GetTested.Extract
import GetTested.Types
import Paths_get_tested (version)

main :: IO ()
main = do
  cmd <- execParser (parser `withInfo` "Generate a test matrix from the tested-with stanza of your cabal file")
  processingResult <- runCLIEff $ case cmd of
    CheckCommand options -> check options
    GenerateCommand options -> generate options
  case processingResult of
    Right () -> pure ()
    Left (CabalFileNotFound path) -> do
      putStrLn $ "get-tested: Could not find cabal file at path " <> path
      exitFailure
    Left (CabalFileCouldNotBeParsed path) -> do
      putStrLn $ "get-tested: Could not parse cabal file at path " <> path
      exitFailure
    Left (NoCompilerVersionsFound path) -> do
      putStrLn $ "get-tested: No compilers found in " <> path
      exitFailure
    Left (IncompatibleOptions opt1 opt2) -> do
      putStrLn $ Text.unpack $ "get-tested: Incompatible options: " <> opt1 <> " and " <> opt2 <> " cannot be passed simultaneously."
      exitFailure
    Left (VersionCheckFailed path failures) -> do
      putStrLn $
        "get-tested: Check for "
          <> path
          <> " failed:"
          <> foldMap (\compiler -> "\n  " <> prettyShow compiler) failures
      exitFailure

parser :: Parser Command
parser =
  ( hsubparser (command "check" checkCommandParserInfo)
      <|> (GenerateCommand <$> generateOptionsParser)
  )
    <**> simpleVersioner (Data.Version.showVersion version)

checkCommandParserInfo :: ParserInfo Command
checkCommandParserInfo =
  info
    (CheckCommand <$> checkOptionsParser)
    (progDesc "Check whether some versions are a subset of the tested versions or not")

checkOptionsParser :: Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> (optional . strOption)
      ( long "from"
          <> metavar "FILE"
          <> help "Include the tested versions of this Cabal file"
          <> action "file"
      )
    <*> strArgument (metavar "FILE" <> action "file")
    <*> (fmap Vector.fromList . many . argument versionReader)
      ( metavar "VERSION"
          <> help "Check if this version is one of the tested versions"
      )

generateOptionsParser :: Parser GenerateOptions
generateOptionsParser =
  GenerateOptions
    <$> strArgument (metavar "FILE" <> action "file")
    <*> switch (long "macos" <> help "(legacy) Enable the macOS runner's latest version")
    <*> optional (strOption (long "macos-version" <> metavar "VERSION" <> help "Enable the macOS runner with the selected version"))
    <*> switch (long "ubuntu" <> help "(legacy) Enable the Ubuntu runner's latest version")
    <*> optional (strOption (long "ubuntu-version" <> metavar "VERSION" <> help "Enable the Ubuntu runner with the selected version"))
    <*> switch (long "windows" <> help "(legacy) Enable the Windows runner's latest version")
    <*> optional (strOption (long "windows-version" <> metavar "VERSION" <> help "Enable the Windows runner with the selected version"))
    <*> switch (long "newest" <> help "Enable only the newest GHC version found in the cabal file")
    <*> switch (long "oldest" <> help "Enable only the oldest GHC version found in the cabal file")

check
  :: (Console :> es, Error ProcessingError :> es, FileSystem :> es)
  => CheckOptions -> Eff es ()
check options = do
  compilers <- extractTestedWith <$> loadFile options.checkOptionsPath

  failures <- case options.checkOptionsFrom of
    Nothing -> pure $ Vector.filter (`Vector.notElem` compilers) options.checkOptionsVersions
    Just fp -> do
      versionsFromFile <- extractTestedWith <$> loadFile fp
      pure $ Vector.filter (`Vector.notElem` versionsFromFile) compilers

  case NEVector.fromVector failures of
    Nothing -> pure ()
    Just failures' ->
      throwError $ VersionCheckFailed options.checkOptionsPath failures'

generate
  :: (Console :> es, Error ProcessingError :> es, FileSystem :> es)
  => GenerateOptions -> Eff es ()
generate options = do
  checkIncompatibleRelativeOptions options
  genericPackageDescription <- loadFile options.generateOptionsPath
  selectedCompilers <-
    filterCompilers options
      <$> extractNonEmptyTestedWith options.generateOptionsPath genericPackageDescription
  let filteredList =
        processOSFlag MacOS options.generateOptionsMacosFlag options.generateOptionsMacosVersion
          <> processOSFlag Ubuntu options.generateOptionsUbuntuFlag options.generateOptionsUbuntuVersion
          <> processOSFlag Windows options.generateOptionsWindowsFlag options.generateOptionsWindowsVersion
  Console.Lazy.putStrLn $
    if null filteredList
      then Aeson.encode selectedCompilers
      else
        let include = PlatformAndVersion <$> filteredList <*> selectedCompilers
         in "matrix=" <> Aeson.encode (ActionMatrix include)

versionReader :: ReadM Version
versionReader =
  eitherReader $
    either (Left . show) Right
      . runParsecParser parsec "<versionReader>"
      . fieldLineStreamFromString

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
  => GenerateOptions
  -> Eff es ()
checkIncompatibleRelativeOptions options = do
  when (options.generateOptionsNewest && options.generateOptionsOldest) $
    throwError $
      IncompatibleOptions
        "--newest"
        "--oldest"

runCLIEff
  :: Eff [Console, FileSystem, Error ProcessingError, IOE] a
  -> IO (Either ProcessingError a)
runCLIEff run = do
  run
    & Console.runConsole
    & FileSystem.runFileSystem
    & Error.runErrorNoCallStack
    & runEff
