module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (
  Val(..),
  -- Functions
  eval,
  envEmpty,
  envLookup,
  envExtend,
  -- Errors
  divByZeroErr,
  negExpErr,
  ifErr,
  eqlErr,
  lookupErr,
  envEmpty,
--   arithBoolErr,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

expConstructorTests :: TestTree
expConstructorTests =
  testGroup
  "Testing Exp constructors"
  [
    testCase "Testing Exp constructor of Integer" $ eval envEmpty (CnstInt 1) @?= Right (ValInt 1),
    testCase "Testing Exp constructor of Bool" $ eval envEmpty (CnstBool True) @?= Right (ValBool True)
  ]
evalArithmeticTests :: TestTree
evalArithmeticTests =
  testGroup
  "Testing evaluating arithmetic expressions"
  [
    -- Test Add
    testCase "Testing eval correctly adds 2+2=4" $ eval envEmpty (Add (CnstInt 2) (CnstInt 2)) @?= Right (ValInt 4),
    testCase "Testing eval correctly add (-2)+2=0" $ eval envEmpty (Add (CnstInt (-2)) (CnstInt 2)) @?= Right (ValInt 0),
    -- Test Sub
    testCase "Testing eval correctly subtracts 2-2=0" $ eval envEmpty (Sub (CnstInt 2) (CnstInt 2)) @?= Right (ValInt 0),
    testCase "Testing eval correctly subtracts 2-4=-2" $ eval envEmpty (Sub (CnstInt 2) (CnstInt 4)) @?= Right (ValInt (-2)),
    -- Test Mul
    testCase "Testing eval correctly multiplies 4*4=16" $ eval envEmpty (Mul (CnstInt 4) (CnstInt 4)) @?= Right (ValInt 16),
    -- Test Div
    testCase "Testing eval correctly divides 12/3=4" $ eval envEmpty (Div (CnstInt 12) (CnstInt 3)) @?= Right (ValInt 4),
    testCase "Testing eval raises div-by-zero error" $ eval envEmpty (Div (CnstInt 1) (CnstInt 0)) @?= Left divByZeroErr,
    -- Test Pow
    testCase "Testing eval correctly applies power 2^3=8" $ eval envEmpty (Pow (CnstInt 2) (CnstInt 3)) @?= Right (ValInt 8),
    testCase "Testing eval raises neg-exp error" $ eval envEmpty (Pow (CnstInt 1) (CnstInt (-1))) @?= Left negExpErr
  ]
evalConditionalsTests :: TestTree
evalConditionalsTests =
  testGroup
  "Testing evaluating conditional expressions"
  [
    -- Test Eval Bool
    testCase "Testing boolean constructor returns bool" $ eval envEmpty (CnstBool True) @?= Right (ValBool True),
    -- Test Eql (Exp)
    testCase "Testing ValBool equality condition True == True" $ eval envEmpty (Eql (CnstBool True) (CnstBool True)) @?= Right (ValBool True),
    testCase "Testing ValBool equality condition True == False" $ eval envEmpty (Eql (CnstBool True) (CnstBool False)) @?= Right (ValBool False),
    testCase "Testing ValBool equality condition False == False" $ eval envEmpty (Eql (CnstBool False) (CnstBool False)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x == x" $ eval envEmpty (Eql (CnstInt 1) (CnstInt 1)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x /= y" $ eval envEmpty (Eql (CnstInt 1) (CnstInt 2)) @?= Right (ValBool False),
    testCase "Testing equality type mismatch error" $ eval envEmpty (Eql (CnstInt 1) (CnstBool True)) @?= Left eqlErr,
    -- Test If (Exp)
    testCase "Testing conditional If ValInt error" $ eval envEmpty (If (CnstInt 1) (CnstInt 1) (CnstInt 1)) @?= Left ifErr,
    testCase "Testing conditional If `error` error" $ eval envEmpty (If (Div (CnstInt 1) (CnstInt 0)) (CnstInt 1) (CnstInt 1)) @?= Left divByZeroErr,
    testCase "Testing conditional If `True`" $ eval envEmpty (If (CnstBool True) (CnstInt 3) (CnstInt 1)) @?= Right (ValInt 3),
    testCase "Testing conditional If `False`" $ eval envEmpty (If (CnstBool False) (CnstInt 3) (CnstInt 1)) @?= Right (ValInt 1)
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
    testCase "Let 'x'=3 evaluate x+x=6" $ eval envEmpty (Let ("x") (CnstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
    testCase "Let 'x'=3 evaluate x+y=5 for y=2" $ eval [("y", ValInt 2)] (Let ("x") (CnstInt 3) (Add (Var "x") (Var "y"))) @?= Right (ValInt 5),
    testCase "Error when first expression errors" $ eval envEmpty (Let ("x") (Pow (CnstInt 1) (CnstInt (-1))) (Add (Var "x") (Var "y"))) @?= Left negExpErr 
  ]

tests :: TestTree
tests =
  testGroup
    "Evaluation tests"
    [
      expConstructorTests,
      evalConditionalsTests,
      evalArithmeticTests,
      evalEnvTests
    ]
