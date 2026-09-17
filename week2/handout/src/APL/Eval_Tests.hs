module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval, EvalM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))



tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      testCase "Construct EvalM (Env -> Eithor Error ValInt 1)" $
        runEval (eval (CstInt 1)) @?= Right (ValInt 1) -- (Env -> Right a)
    ]
