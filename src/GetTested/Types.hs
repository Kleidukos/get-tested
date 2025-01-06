{-# OPTIONS_GHC -Wno-orphans #-}

module GetTested.Types where

import Data.Aeson
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Distribution.Compiler
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Version
import Distribution.Types.VersionRange
import GHC.Generics (Generic)

data ProcessingError
  = CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath String
  | NoCompilerVersionsFound FilePath
  | IncompatibleOptions Text Text
  | VersionCheckFailed FilePath (NonEmptyVector Version)
  deriving stock (Eq, Ord, Show)

instance Display ProcessingError where
  displayBuilder (CabalFileNotFound path) =
    "Could not find cabal file at path " <> fromString path
  displayBuilder (CabalFileCouldNotBeParsed path err) =
    ("Could not parse cabal file at path " <> fromString path <> ": ")
      <> fromString err
  displayBuilder (IncompatibleOptions opt1 opt2) =
    "Incompatible options: "
      <> Builder.fromText opt1
      <> " and "
      <> Builder.fromText opt2
      <> " cannot be passed simultaneously."
  displayBuilder (NoCompilerVersionsFound path) =
    "No compilers found in " <> fromString path
  displayBuilder (VersionCheckFailed path failures) =
    ("Check for " <> fromString path <> " failed:")
      <> foldMap (\compiler -> "\n  " <> displayBuilder compiler) failures

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
  displayBuilder = fromString . Pretty.prettyShow

instance Display VersionRange where
  displayBuilder = fromString . Pretty.prettyShow

instance Display Version where
  displayBuilder = fromString . Pretty.prettyShow

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
