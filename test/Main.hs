module Main (main) where

import GHC.List (List)
import System.IO
import Test.Tasty
import Test.Tasty.Runners.Reporter qualified as Reporter

import ExtractTest qualified
import Utils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  spec <- traverse (\comp -> runTestEff comp) specs
  defaultMainWithIngredients
    [Reporter.ingredient]
    $ testGroup
      "Confer Tests"
      spec

specs :: List (TestEff TestTree)
specs =
  [ ExtractTest.spec
  ]
