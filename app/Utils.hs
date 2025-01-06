module Utils where

import Distribution.Parsec (parsec, runParsecParser)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)
import Distribution.Types.Version (Version)
import Options.Applicative

import GetTested.CLI.Types

data Command
  = CheckCommand CheckOptions
  | GenerateCommand GenerateOptions
  | LegacyDefault GenerateOptions

versionReader :: ReadM Version
versionReader =
  eitherReader $
    either (Left . show) Right
      . runParsecParser parsec "<versionReader>"
      . fieldLineStreamFromString
