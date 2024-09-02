module Utils where

import Data.Function ((&))
import Effectful
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import GHC.Stack
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import GetTested.Types

type TestEff =
  Eff [Console, FileSystem, Error ProcessingError, IOE]

runTestEff
  :: Eff [Console, FileSystem, Error ProcessingError, IOE] a
  -> IO a
runTestEff action = do
  result <-
    action
      & Console.runConsole
      & FileSystem.runFileSystem
      & Error.runErrorNoCallStack
      & runEff
  case result of
    Left e -> Test.assertFailure $ show e
    Right a -> pure a

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  let test = runTestEff assertion
  pure $
    Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

assertEqual
  :: (Eq a, Show a, HasCallStack)
  => String
  -> a
  -- ^ Expected
  -> a
  -- ^ Actual
  -> TestEff ()
assertEqual label expected actual = liftIO $ Test.assertEqual label expected actual
