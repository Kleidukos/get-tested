module Command.Check
  ( checkCommandParserInfo
  , check
  ) where

import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem (FileSystem)
import Options.Applicative

import GetTested.CLI.Types
import GetTested.Extract
import GetTested.Types
import Utils

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
    <*> strArgument
      ( metavar "FILE"
          <> action "file"
      )
    <*> (fmap Vector.fromList . many . argument versionReader)
      ( metavar "VERSION"
          <> help "Check if this version is one of the tested versions"
      )

check
  :: (Error ProcessingError :> es, FileSystem :> es)
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
