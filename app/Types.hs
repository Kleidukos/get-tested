{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Data.Aeson
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Builder
import Distribution.Compiler
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Version qualified as Version
import Data.Vector (Vector)
import GHC.Generics (Generic)

data ProcessingError
  = CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath
  deriving stock (Eq, Ord, Show)
  deriving
    (Display)
    via ShowInstance ProcessingError

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
  { include :: Vector Version
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)

versionList :: Set Version
versionList =
  Set.fromList
    [ Version.mkVersion [9, 4, 1]
    , Version.mkVersion [9, 2, 4]
    , Version.mkVersion [9, 2, 3]
    , Version.mkVersion [9, 2, 2]
    , Version.mkVersion [9, 2, 1]
    , Version.mkVersion [9, 0, 2]
    , Version.mkVersion [9, 0, 1]
    , Version.mkVersion [8, 10, 7]
    , Version.mkVersion [8, 10, 6]
    , Version.mkVersion [8, 10, 5]
    , Version.mkVersion [8, 10, 4]
    , Version.mkVersion [8, 10, 3]
    , Version.mkVersion [8, 10, 2]
    , Version.mkVersion [8, 10, 1]
    , Version.mkVersion [8, 8, 4]
    , Version.mkVersion [8, 8, 3]
    , Version.mkVersion [8, 8, 2]
    , Version.mkVersion [8, 8, 1]
    , Version.mkVersion [8, 6, 5]
    , Version.mkVersion [8, 6, 4]
    , Version.mkVersion [8, 6, 3]
    , Version.mkVersion [8, 6, 2]
    , Version.mkVersion [8, 6, 1]
    , Version.mkVersion [8, 4, 4]
    , Version.mkVersion [8, 4, 3]
    , Version.mkVersion [8, 4, 2]
    , Version.mkVersion [8, 4, 1]
    , Version.mkVersion [8, 2, 2]
    , Version.mkVersion [8, 0, 2]
    , Version.mkVersion [7, 10, 3]
    ]
