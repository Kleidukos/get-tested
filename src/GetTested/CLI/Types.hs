module GetTested.CLI.Types where

import Data.Text (Text)

data Options = Options
  { path :: FilePath
  , macosFlag :: Bool
  , macosVersion :: Maybe Text
  , ubuntuFlag :: Bool
  , ubuntuVersion :: Maybe Text
  , windowsFlag :: Bool
  , windowsVersion :: Maybe Text
  , newest :: Bool
  , oldest :: Bool
  }
  deriving stock (Show, Eq)
