module GetTested.CLI.Types where

import Data.Text (Text)
import Data.Vector (Vector)
import Distribution.Types.Version (Version)

data CheckOptions = CheckOptions
  { checkOptionsFrom :: Maybe FilePath
  , checkOptionsPath :: FilePath
  , checkOptionsVersions :: Vector Version
  }
  deriving stock (Show, Eq)

data GenerateOptions = GenerateOptions
  { generateOptionsPath :: FilePath
  , generateOptionsMacosFlag :: Bool
  , generateOptionsMacosVersion :: Maybe Text
  , generateOptionsUbuntuFlag :: Bool
  , generateOptionsUbuntuVersion :: Maybe Text
  , generateOptionsWindowsFlag :: Bool
  , generateOptionsWindowsVersion :: Maybe Text
  , generateOptionsNewest :: Bool
  , generateOptionsOldest :: Bool
  }
  deriving stock (Show, Eq)
