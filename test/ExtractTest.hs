module ExtractTest where

import Data.Vector qualified as Vector
import Distribution.Types.Version qualified as Version
import Test.Tasty (TestTree)

import GetTested.CLI.Types
import GetTested.Extract qualified as Extract
import Utils

spec :: TestEff TestTree
spec =
  testThese
    "Symlink Checks"
    [ testThis "Filter the newest compiler" testFilteringNewestCompiler
    , testThis "Filter the oldest compiler" testFilteringOldestCompiler
    ]

testFilteringNewestCompiler :: TestEff ()
testFilteringNewestCompiler = do
  let options =
        GenerateOptions
          { generateOptionsPath = "./test/fixtures/one.cabal"
          , generateOptionsMacosFlag = False
          , generateOptionsMacosVersion = Nothing
          , generateOptionsUbuntuFlag = False
          , generateOptionsUbuntuVersion = Just "latest"
          , generateOptionsWindowsFlag = False
          , generateOptionsWindowsVersion = Nothing
          , generateOptionsNewest = True
          , generateOptionsOldest = False
          }
  genericPackageDescription <- Extract.loadFile "./test/fixtures/one.cabal"
  result <-
    Extract.filterCompilers options
      <$> Extract.extractNonEmptyTestedWith options.generateOptionsPath genericPackageDescription
  assertEqual
    ""
    (Vector.singleton $ Version.mkVersion [9, 10, 1])
    result

testFilteringOldestCompiler :: TestEff ()
testFilteringOldestCompiler = do
  let options =
        GenerateOptions
          { generateOptionsPath = "./test/fixtures/one.cabal"
          , generateOptionsMacosFlag = False
          , generateOptionsMacosVersion = Nothing
          , generateOptionsUbuntuFlag = False
          , generateOptionsUbuntuVersion = Just "latest"
          , generateOptionsWindowsFlag = False
          , generateOptionsWindowsVersion = Nothing
          , generateOptionsNewest = False
          , generateOptionsOldest = True
          }
  genericPackageDescription <- Extract.loadFile "./test/fixtures/one.cabal"
  result <-
    Extract.filterCompilers options
      <$> Extract.extractNonEmptyTestedWith options.generateOptionsPath genericPackageDescription
  assertEqual
    ""
    (Vector.singleton $ Version.mkVersion [8, 10, 7])
    result
