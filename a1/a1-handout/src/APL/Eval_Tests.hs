module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (
  Val(..),
  -- Functions
  eval,
  envEmpty,
  -- Errors
  divByZeroErr,
  negExpErr,
  ifErr,
  eqlErr,
  lookupErr,
  envEmpty,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

expConstructorTests :: TestTree
expConstructorTests =
  testGroup
  "Testing Exp constructors"
  [
    testCase "Testing Exp constructor of Integer" $ eval envEmpty (CstInt 1) @?= Right (ValInt 1),
    testCase "Testing Exp constructor of Bool" $ eval envEmpty (CstBool True) @?= Right (ValBool True)
  ]
evalArithmeticTests :: TestTree
evalArithmeticTests =
  testGroup
  "Testing evaluating arithmetic expressions"
  [
    -- Test Add
    testCase "Testing eval correctly adds 2+2=4" $ eval envEmpty (Add (CstInt 2) (CstInt 2)) @?= Right (ValInt 4),
    testCase "Testing eval correctly add (-2)+2=0" $ eval envEmpty (Add (CstInt (-2)) (CstInt 2)) @?= Right (ValInt 0),
    -- Test Sub
    testCase "Testing eval correctly subtracts 2-2=0" $ eval envEmpty (Sub (CstInt 2) (CstInt 2)) @?= Right (ValInt 0),
    testCase "Testing eval correctly subtracts 2-4=-2" $ eval envEmpty (Sub (CstInt 2) (CstInt 4)) @?= Right (ValInt (-2)),
    -- Test Mul
    testCase "Testing eval correctly multiplies 4*4=16" $ eval envEmpty (Mul (CstInt 4) (CstInt 4)) @?= Right (ValInt 16),
    -- Test Div
    testCase "Testing eval correctly divides 12/3=4" $ eval envEmpty (Div (CstInt 12) (CstInt 3)) @?= Right (ValInt 4),
    testCase "Testing eval raises div-by-zero error" $ eval envEmpty (Div (CstInt 1) (CstInt 0)) @?= Left divByZeroErr,
    -- Test Pow
    testCase "Testing eval correctly applies power 2^3=8" $ eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
    testCase "Testing eval raises neg-exp error" $ eval envEmpty (Pow (CstInt 1) (CstInt (-1))) @?= Left negExpErr
  ]
evalConditionalsTests :: TestTree
evalConditionalsTests =
  testGroup
  "Testing evaluating conditional expressions"
  [
    -- Test Eval Bool
    testCase "Testing boolean constructor returns bool" $ eval envEmpty (CstBool True) @?= Right (ValBool True),
    -- Test Eql (Exp)
    testCase "Testing ValBool equality condition True == True" $ eval envEmpty (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True),
    testCase "Testing ValBool equality condition True == False" $ eval envEmpty (Eql (CstBool True) (CstBool False)) @?= Right (ValBool False),
    testCase "Testing ValBool equality condition False == False" $ eval envEmpty (Eql (CstBool False) (CstBool False)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x == x" $ eval envEmpty (Eql (CstInt 1) (CstInt 1)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x /= y" $ eval envEmpty (Eql (CstInt 1) (CstInt 2)) @?= Right (ValBool False),
    testCase "Testing equality type mismatch error" $ eval envEmpty (Eql (CstInt 1) (CstBool True)) @?= Left eqlErr,
    -- Test If (Exp)
    testCase "Testing conditional If ValInt error" $ eval envEmpty (If (CstInt 1) (CstInt 1) (CstInt 1)) @?= Left ifErr,
    testCase "Testing conditional If `error` error" $ eval envEmpty (If (Div (CstInt 1) (CstInt 0)) (CstInt 1) (CstInt 1)) @?= Left divByZeroErr,
    testCase "Testing conditional If `True`" $ eval envEmpty (If (CstBool True) (CstInt 3) (CstInt 1)) @?= Right (ValInt 3),
    testCase "Testing conditional If `False`" $ eval envEmpty (If (CstBool False) (CstInt 3) (CstInt 1)) @?= Right (ValInt 1)
  ]
evalEnvTests :: TestTree
evalEnvTests =
  testGroup
  "Testing evaluating on environments (?)"
  [
    -- Test eval env Var
    testCase "Testing lookup on empty environment error" $ eval envEmpty (Var "") @?= Left lookupErr,
    testCase "Testing failed lookup" $ eval [("x", ValBool True)] (Var "y") @?= Left (lookupErr ++ "y"),
    testCase "Testing succesful lookup" $ eval [("x", ValBool True)] (Var "x") @?= Right (ValBool True),
    -- Test eval env Let
    testCase "Let 'x'=3 evaluate x+x=6" $ eval envEmpty (Let ("x") (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
    testCase "Let 'x'=3 evaluate x+y=5 for y=2" $ eval [("y", ValInt 2)] (Let ("x") (CstInt 3) (Add (Var "x") (Var "y"))) @?= Right (ValInt 5),
    testCase "Error when first expression errors" $
      eval envEmpty (Let ("x") (Pow (CstInt 1) (CstInt (-1))) (Add (Var "x") (Var "y"))) @?= Left negExpErr ,
    testCase "Let (shadowing)" $
      eval
        envEmpty
        ( Let
            "x"
            (Add (CstInt 2) (CstInt 3))
            (Let "x" (CstBool True) (Var "x"))
        )
        @?= Right (ValBool True)
  ]
evalForLoopTests :: TestTree
evalForLoopTests = 
  testGroup
  "Testing For loop implementation"
  [
    -- Test handout example 
    testCase "Example from handout" $ eval envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i"))) @?= Right (ValInt 45)
  ]


tests :: TestTree
tests =
  testGroup
    "Evaluation tests"
    [
      expConstructorTests,
      evalConditionalsTests,
      evalArithmeticTests,
      evalEnvTests,
      evalForLoopTests
    ]
