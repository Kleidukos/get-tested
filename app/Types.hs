{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Builder
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Compiler
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Version
import Distribution.Types.VersionRange
import GHC.Generics (Generic)

data ProcessingError
  = CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath
  deriving stock (Eq, Ord, Show)
  deriving
    (Display)
    via ShowInstance ProcessingError

data RunnerOS
  = Ubuntu
  | Windows
  | MacOS
  deriving stock (Eq, Ord, Show)

instance Display RunnerOS where
  displayBuilder Ubuntu = "ubuntu"
  displayBuilder Windows = "windows"
  displayBuilder MacOS = "macos"

instance ToJSON CompilerFlavor where
  toJSON = toJSON . Pretty.prettyShow

instance FromJSON CompilerFlavor where
  parseJSON = withText "Compiler Flavor" $ \s ->
    maybe (fail "Invalid compiler flavor") pure (simpleParsec $ Text.unpack s)

instance Display CompilerFlavor where
  displayBuilder = Builder.fromString . Pretty.prettyShow

instance Display VersionRange where
  displayBuilder = Builder.fromString . Pretty.prettyShow

instance ToJSON Version where
  toJSON = toJSON . Pretty.prettyShow

instance FromJSON VersionRange where
  parseJSON = withText "Version Range" $ \s ->
    maybe (fail "Invalid version range") pure (simpleParsec $ Text.unpack s)

data ActionMatrix = ActionMatrix
  { include :: Vector PlatformAndVersion
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)

data PlatformAndVersion = PlatformAndVersion
  { os :: Text
  , ghc :: Version
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)

osList :: Vector Text
osList =
  Vector.fromList
    [ "ubuntu-latest"
    , "macos-latest"
    , "windows-latest"
    ]
