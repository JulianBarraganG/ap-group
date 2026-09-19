module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
      testPos (CstInt 2),
      testNeg(Var ("x")),
      testPos (Add (CstInt 2) (CstInt 2)),
      testNeg (Add (CstInt 2) (Var "x")),

      testNeg (Add (Let "x" (CstInt 1) (Var "x")) (Var "x")), --No leakage

      testNeg (If (CstBool True) (CstInt 3) (Var "x")),

      testPos (If (CstInt 3) (CstBool False) (CstBool True)),

      testPos (Let "x" (CstInt 3) (Add (Var "x") (CstInt 5))),
      testNeg (Let "x" (Add (Var "x") (CstInt 5)) (CstInt 3)),
      testPos (Let "x" (CstInt 1) (Lambda "y" (Add (Var "x") (Var "y")))),

      testPos (ForLoop ("p", (CstInt 0)) ("i", (CstInt 10)) (Add (Var "p") (Var "i"))),
      testNeg (ForLoop ("p", (Add (Var "i") (CstInt 2))) ("i", (CstInt 10)) (Add (Var "p") (Var "i"))),
      testNeg (ForLoop ("p", (Add (Var "p") (CstInt 2))) ("i", (CstInt 10)) (Add (Var "p") (Var "i"))),

      testPos (Lambda "x" (CstBool True)),
      testPos (Lambda "x" (Add (Var "x") (CstInt 2))),
      testNeg (Lambda "y" (Add (Var "x") (CstInt 2))),

      testPos (Apply (CstInt 3) (CstInt 3)), --Wouldn't evaluate since CstInt doesn't evaluate to a ValFun, 
      --but typechecker obviously only cares about whether variables are in scope
      --
      testPos (Apply (Lambda "x" (Add (Var "x") (CstInt 2))) (CstInt 3)),
      testPos (Apply (Lambda "x" (Add (Var "x") (CstInt 2))) (CstBool True)), -- also wouldn't eval, check is fine
      testNeg (Apply (Lambda "x" (Add (Var "y") (CstInt 2))) (CstInt 3)),
      testNeg (Apply (Lambda "x" (Add (Var "x") (CstInt 2))) (Var "y")),

      testPos (TryCatch (CstInt 2) (CstBool True)),
      testNeg (TryCatch (CstInt 2) (Var "x")), -- fails, even though e2 wouldn't be evaluated

      testNeg (Print "x" (Var "x")), --string isn't a var

      testNeg (KvPut(Apply (Lambda "x" (Add (Var "x") (CstInt 2))) (CstBool True)) (Var "x")), --e1 e2 have different envs. 

      testPos (KvPut (CstInt 1) (CstBool True)), 

      testPos (KvGet (CstInt 1)), 
      testNeg (KvGet (Var "x"))
    ]
