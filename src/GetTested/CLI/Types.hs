module GetTested.CLI.Types where

import Data.Text (Text)
import Data.Vector (Vector)

data Options = Options
  { path :: FilePath
  , macosFlag :: Bool
  , macosVersion :: Vector Text
  , ubuntuFlag :: Bool
  , ubuntuVersion :: Vector Text
  , windowsFlag :: Bool
  , windowsVersion :: Vector Text
  , newest :: Bool
  , oldest :: Bool
  }
  deriving stock (Show, Eq)
