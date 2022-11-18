module Main where

import Effectful
import Effectful.Error.Static
import Extract
import Options.Applicative
import Types
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString

newtype Options = Options {path :: FilePath}
  deriving stock (Show, Eq)

main :: IO ()
main = do
  result <- execParser (parseOptions `withInfo` "Extract the tested-with stanza from your cabal file")
  processingResult <- runEff . runErrorNoCallStack $ runOptions result
  case processingResult of
    Right json -> ByteString.putStrLn json
    Left _ -> error "mleh!"

parseOptions :: Parser Options
parseOptions =
  Options
    <$> argument str (metavar "FILE")

runOptions :: Options -> Eff [Error ProcessingError, IOE] ByteString
runOptions options = do
  genericPackageDescription <- loadFile options.path
  let supportedCompilers = extractTestedWith genericPackageDescription
      result = getVersions supportedCompilers
  pure $ Aeson.encode $ ActionMatrix result

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
