module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (Val(..), eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

evalTest :: TestTree
evalTest = testCase "Testing eval func" $ eval (CnstInt 1) @?= ValInt 1
tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      evalTest
    ]
