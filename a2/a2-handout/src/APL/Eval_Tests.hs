module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, State, Val (..), eval, runEval, stateEmpty, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> (State, Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= (stateEmpty, Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= (stateEmpty, Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= (stateEmpty, Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= (stateEmpty, Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= (stateEmpty, Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= (stateEmpty, Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= (stateEmpty, Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= (stateEmpty, Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= (stateEmpty, Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= (stateEmpty, Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= (stateEmpty, Right (ValInt 2)),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= (stateEmpty, Right (ValInt 5)),
      --
      testCase "ForLoop" $
        eval'
          (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= (stateEmpty, Right (ValInt 45)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= (stateEmpty, Right (ValBool True)),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= (stateEmpty, Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= (stateEmpty, Right (ValBool True))
    ]

printTests :: TestTree
printTests =
  testGroup
    "Task 1: Printing"
    [
      testCase "Print 'foo' int 2 returns updated state and val 2" $
        eval' (Print "foo" (CstInt 2))
        @?= (["foo: 2"], Right (ValInt 2)),

      testCase "Print 'foo' Add (2 2) returns updated state and val 4" $
        eval' (Print "foo" (Add (CstInt 2) (CstInt 2)))
        @?= (["foo: 4"], Right (ValInt 4)),


     testCase "Print 'bar' True returns a state containing 'bar: True'" $
       eval' (Print "bar" (CstBool True))
       @?= (["bar: True"], Right (ValBool True)),

     testCase "Print with an expression evaluation to an error returns the error" $
       eval' (Print "foo" (Div (CstInt 2) (CstInt 0)))
       @?= ([], Left "Division by zero"),

      testCase "Print with a fun returns '#<fun>'" $
        eval' (Print "fun" (Lambda "x" (Mul (Var "x") (Var "x"))))
        @?= (["fun: #<fun>"], Right $ ValFun envEmpty "x" (Mul (Var "x") (Var "x"))),

      testCase "First string to be printed is first in the list" $
        eval' ( Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3)) 
        @?= (["foo: 2","bar: 3"],Right (ValInt 3)),
      
      testCase "Good Print expressions are added, even though full expression fails" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Div (CstInt 1) (CstInt 0)))
        @?= (["foo: 2"], Left "Division by zero")
    ]

kvTests :: TestTree
kvTests =
  testGroup
    "Task 2: Key-value store"
    []

tests :: TestTree
tests = testGroup "Evaluation" [evalTests, printTests]--, kvTests]
