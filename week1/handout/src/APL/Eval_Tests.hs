module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (
  Val(..),
  eval,
  divByZeroErr,
  negExpErr,
  arithBoolErr,
  eqlErr,
  ifErr,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Test for Eq eval(Exp) = Val of same integer
evalEqTest :: TestTree
evalEqTest = testCase "Testing eval returns correct value" $ eval (CnstInt 1) @?= Right (ValInt 1)
-- Test Add
evalAddTest :: TestTree
evalAddTest = testCase "Testing eval correctly adds 2+2=4" $ eval (Add (CnstInt 2) (CnstInt 2)) @?= Right (ValInt 4)
evalNegAddTest :: TestTree -- negative Add
evalNegAddTest = testCase "Testing eval correctly add (-2)+2=0" $ eval (Add (CnstInt (-2)) (CnstInt 2)) @?= Right (ValInt 0)
-- Test Sub
evalSubTest :: TestTree
evalSubTest = testCase "Testing eval correctly subtracts 2-2=0" $ eval (Sub (CnstInt 2) (CnstInt 2)) @?= Right (ValInt 0)
evalNegSubTest :: TestTree -- negative Val from sub
evalNegSubTest = testCase "Testing eval correctly subtracts 2-4=-2" $ eval (Sub (CnstInt 2) (CnstInt 4)) @?= Right (ValInt (-2))
-- Test Mul
evalMulTest :: TestTree
evalMulTest = testCase "Testing eval correctly multiplies 4*4=16" $ eval (Mul (CnstInt 4) (CnstInt 4)) @?= Right (ValInt 16)
-- Test Div
evalDivTest :: TestTree
evalDivTest = testCase "Testing eval correctly divides 12/3=4" $ eval (Div (CnstInt 12) (CnstInt 3)) @?= Right (ValInt 4)
evalDivByZeroTest :: TestTree
evalDivByZeroTest = testCase "Testing eval raises div-by-zero error" $ eval (Div (CnstInt 1) (CnstInt 0)) @?= Left divByZeroErr
-- Test Pow
evalPowTest :: TestTree
evalPowTest = testCase "Testing eval correctly applies power 2^3=8" $ eval(Pow (CnstInt 2) (CnstInt 3)) @?= Right (ValInt 8)
evalNegExpPowTest :: TestTree
evalNegExpPowTest = testCase "Testing eval raises neg-exp error" $ eval (Pow (CnstInt 1) (CnstInt (-1))) @?= Left negExpErr
-- Test Eval Bool
evalBoolTest :: TestTree
evalBoolTest = testCase "Testing boolean constructor returns bool" $ eval (CnstBool True) @?= Right (ValBool True)
-- Test Eql (Exp)
evalEqlBoolTest1 :: TestTree
evalEqlBoolTest1 = testCase "Testing ValBool equality condition True == True" $ eval (Eql (CnstBool True) (CnstBool True)) @?= Right (ValBool True)
evalEqlBoolTest2 :: TestTree
evalEqlBoolTest2 = testCase "Testing ValBool equality condition True == False" $ eval (Eql (CnstBool True) (CnstBool False)) @?= Right (ValBool False)
evalEqlBoolTest3 :: TestTree
evalEqlBoolTest3 = testCase "Testing ValBool equality condition False == False" $ eval (Eql (CnstBool False) (CnstBool False)) @?= Right (ValBool True)
evalEqlIntTest1 :: TestTree
evalEqlIntTest1 = testCase "Testing ValInt equality condition x == x" $ eval (Eql (CnstInt 1) (CnstInt 1)) @?= Right (ValBool True)
evalEqlIntTest2 :: TestTree
evalEqlIntTest2 = testCase "Testing ValInt equality condition x /= y" $ eval (Eql (CnstInt 1) (CnstInt 2)) @?= Right (ValBool False)
evalEqlErrTest :: TestTree
evalEqlErrTest = testCase "Testing equality type mismatch error" $ eval (Eql (CnstInt 1) (CnstBool True)) @?= Left eqlErr
-- Test If (Exp)
evalIfIntErrTest :: TestTree
evalIfIntErrTest = testCase "Testing conditional If ValInt error" $ eval (If (CnstInt 1) (CnstInt 1) (CnstInt 1)) @?= Left ifErr
evalIfErrErrTest :: TestTree
evalIfErrErrTest = testCase "Testing conditional If `error` error" $ eval (If ()

tests :: TestTree
tests =
  testGroup
    "Evaluation tests"
    [
      evalEqTest,
      evalAddTest,
      evalSubTest,
      evalNegSubTest,
      evalNegAddTest,
      evalMulTest,
      evalDivTest,
      evalDivByZeroTest,
      evalPowTest,
      evalNegExpPowTest,
      evalBoolTest,
      evalEqlBoolTest1,
      evalEqlBoolTest2,
      evalEqlBoolTest3,
      evalEqlIntTest1,
      evalEqlIntTest2,
      evalEqlErrTest,
      evalIfIntErrTest
    ]
