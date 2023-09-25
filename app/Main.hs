module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Function ((&))
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Options.Applicative
import System.Exit

import Extract
import Types

data Options = Options
  { path :: FilePath
  , macosFlag :: Bool
  , ubuntuFlag :: Bool
  , windowsFlag :: Bool
  }
  deriving stock (Show, Eq)

main :: IO ()
main = do
  result <- execParser (parseOptions `withInfo` "Extract the tested-with stanza from your cabal file")
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
    <*> switch (long "macos" <> help "Enable the macOS platform")
    <*> switch (long "ubuntu" <> help "Enable the ubuntu platform")
    <*> switch (long "windows" <> help "Enable the windows platform")

runOptions :: Options -> Eff [Error ProcessingError, IOE] ByteString
runOptions options = do
  genericPackageDescription <- loadFile options.path
  let supportedCompilers = extractTestedWith genericPackageDescription
      filteredList =
        osList
          & (if options.macosFlag then id else Vector.filter (/= "macos-latest"))
          & (if options.windowsFlag then id else Vector.filter (/= "windows-latest"))
          & (if options.ubuntuFlag then id else Vector.filter (/= "ubuntu-latest"))
  pure $
    if null filteredList
      then Aeson.encode supportedCompilers
      else do
        let include = PlatformAndVersion <$> filteredList <*> supportedCompilers
        "matrix=" <> Aeson.encode (ActionMatrix include)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
