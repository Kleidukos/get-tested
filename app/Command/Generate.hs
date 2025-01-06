module Command.Generate
  ( generateCommandParserInfo
  , generateOptionsParser
  , generate
  ) where

import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString.Lazy qualified as Console.Lazy
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Options.Applicative

import GetTested.CLI.Types
import GetTested.Extract
import GetTested.Types
import Utils

generateCommandParserInfo :: ParserInfo Command
generateCommandParserInfo =
  info
    (GenerateCommand <$> generateOptionsParser True)
    (progDesc "Generate a test matrix from the tested-with stanza of your Cabal file")

generateOptionsParser
  :: Bool
  -- ^ Whether to display the options in the help text or not
  -> Parser GenerateOptions
generateOptionsParser doDisplay =
  GenerateOptions
    <$> strArgument
      ( metavar "FILE"
          <> action "file"
          <> minternal
      )
    <*> switch
      ( long "macos"
          <> help "(legacy) Enable the macOS runner's latest version"
          <> minternal
      )
    <*> (optional . strOption)
      ( long "macos-version"
          <> metavar "VERSION"
          <> help "Enable the macOS runner with the selected version"
          <> minternal
      )
    <*> switch
      ( long "ubuntu"
          <> help "(legacy) Enable the Ubuntu runner's latest version"
          <> minternal
      )
    <*> (optional . strOption)
      ( long "ubuntu-version"
          <> metavar "VERSION"
          <> help "Enable the Ubuntu runner with the selected version"
          <> minternal
      )
    <*> switch
      ( long "windows"
          <> help "(legacy) Enable the Windows runner's latest version"
          <> minternal
      )
    <*> (optional . strOption)
      ( long "windows-version"
          <> metavar "VERSION"
          <> help "Enable the Windows runner with the selected version"
          <> minternal
      )
    <*> switch
      ( long "newest"
          <> help "Enable only the newest GHC version found in the cabal file"
          <> minternal
      )
    <*> switch
      ( long "oldest"
          <> help "Enable only the oldest GHC version found in the cabal file"
          <> minternal
      )
  where
    minternal :: Mod f a
    minternal
      | doDisplay = idm
      | otherwise = internal

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
