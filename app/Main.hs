module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Extract
import Options.Applicative
import Types

newtype Options = Options {path :: FilePath}
  deriving stock (Show, Eq)

main :: IO ()
main = do
  result <- execParser (parseOptions `withInfo` "Extract the tested-with stanza from your cabal file")
  processingResult <- runEff . runErrorNoCallStack $ runOptions result
  case processingResult of
    Right json -> ByteString.putStrLn $ "matrix=" <> json
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
      include = Vector.map (\version -> PlatformAndVersion "ubuntu-latest" version) result
  pure $ Aeson.encode $ ActionMatrix include

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
