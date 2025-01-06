module Main (main) where

import Data.Function ((&))
import Data.Text.Display (display)
import Data.Text.IO qualified
import Data.Version qualified
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Options.Applicative
import System.Exit

import Command.Check
import Command.Generate
import GetTested.Types
import Paths_get_tested (version)
import Utils

main :: IO ()
main = do
  cmd <- execParser (parser `withInfo` "A utility to work with the tested-with stanza of your cabal files")
  processingResult <- runCLIEff $ case cmd of
    CheckCommand options -> check options
    GenerateCommand options -> generate options
    LegacyDefault options -> do
      -- TODO: Display a warining and suggest migration to `get-tested generate` ?
      generate options
  case processingResult of
    Right () -> pure ()
    Left err -> do
      Data.Text.IO.putStrLn $ "get-tested: " <> display err
      exitFailure

parser :: Parser Command
parser =
  ( hsubparser
      ( command "check" checkCommandParserInfo
          <> metavar "check"
      )
      <|> hsubparser
        ( command "generate" generateCommandParserInfo
            <> metavar "generate"
        )
      <|> (LegacyDefault <$> generateOptionsParser False)
  )
    <**> simpleVersioner (Data.Version.showVersion version)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

runCLIEff
  :: Eff [Console, FileSystem, Error ProcessingError, IOE] a
  -> IO (Either ProcessingError a)
runCLIEff run = do
  run
    & Console.runConsole
    & FileSystem.runFileSystem
    & Error.runErrorNoCallStack
    & runEff
