module APL.Eval_Tests (tests) where

import APL.AST (Exp (..), VName)
import APL.Eval (
  Error,
  Val (..),
  eval,
  runEval,
  -- Errors
  divByZeroErr,
  negExpErr,
  arithNonIntErr,
  eqlErr,
  ifErr,
  lookupErr,
  nonIntegErr,
  notValFunErr
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Evaluate an expression in the given environment and unwrap the
-- result, so tests can compare against a plain 'Either'.
run :: [(VName, Val)] -> Exp -> Either Error Val
run env e = runEval $ eval env e

expConstructorTests :: TestTree
expConstructorTests =
  testGroup
  "Testing Exp constructors"
  [
    testCase "Testing Exp constructor of Integer" $ run [] (CstInt 1) @?= Right (ValInt 1),
    testCase "Testing Exp constructor of Bool" $ run [] (CstBool True) @?= Right (ValBool True)
  ]
evalArithmeticTests :: TestTree
evalArithmeticTests =
  testGroup
  "Testing evaluating arithmetic expressions"
  [
    -- Test Add
    testCase "Testing eval correctly adds 2+2=4" $ run [] (Add (CstInt 2) (CstInt 2)) @?= Right (ValInt 4),
    testCase "Testing eval correctly add (-2)+2=0" $ run [] (Add (CstInt (-2)) (CstInt 2)) @?= Right (ValInt 0),
    testCase "Testing add reacts to nonInt" $ run [] (Add (CstInt 2) (CstBool True)) @?= Left arithNonIntErr,
    -- Test Sub
    testCase "Testing eval correctly subtracts 2-2=0" $ run [] (Sub (CstInt 2) (CstInt 2)) @?= Right (ValInt 0),
    testCase "Testing eval correctly subtracts 2-4=-2" $ run [] (Sub (CstInt 2) (CstInt 4)) @?= Right (ValInt (-2)),
    testCase "Testing sub reacts to nonInt" $ run [] (Sub (CstInt 2) (CstBool True)) @?= Left arithNonIntErr,
    -- Test Mul
    testCase "Testing eval correctly multiplies 4*4=16" $ run [] (Mul (CstInt 4) (CstInt 4)) @?= Right (ValInt 16),
    testCase "Testing eval correctly multiplies 4*0=0" $ run [] (Mul (CstInt 4) (CstInt 0)) @?= Right (ValInt 0),
    testCase "Testing mul reacts to nonInt" $ run [] (Mul (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))) ) @?= Left arithNonIntErr,
    -- Test Div
    testCase "Testing eval correctly divides 12/3=4" $ run [] (Div (CstInt 12) (CstInt 3)) @?= Right (ValInt 4),
    testCase "Testing eval raises div-by-zero error" $ run [] (Div (CstInt 1) (CstInt 0)) @?= Left divByZeroErr,
    testCase "Testing div reacts to nonInt" $ run [] (Div (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))) ) @?= Left arithNonIntErr,
    testCase "Testing div floors" $ run [] (Div (CstInt 5) (CstInt 2)) @?= Right (ValInt 2),
    -- Test Pow
    testCase "Testing eval correctly applies power 2^3=8" $ run [] (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
    testCase "Testing eval correctly applies power 2^0=1" $ run [] (Pow (CstInt 2) (CstInt 0)) @?= Right (ValInt 1),
    testCase "Testing eval raises neg-exp error" $ run [] (Pow (CstInt 1) (CstInt (-1))) @?= Left negExpErr,
    testCase "Testing pow reacts to nonInt" $ run [] (Pow (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))) ) @?= Left arithNonIntErr
  ]
evalConditionalsTests :: TestTree
evalConditionalsTests =
  testGroup
  "Testing evaluating conditional expressions"
  [
    -- Test Eval Bool
    testCase "Testing boolean constructor returns bool" $ run [] (CstBool True) @?= Right (ValBool True),
    -- Test Eql (Exp)
    testCase "Testing ValBool equality condition True == True" $ run [] (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True),
    testCase "Testing ValBool equality condition True == False" $ run [] (Eql (CstBool True) (CstBool False)) @?= Right (ValBool False),
    testCase "Testing ValBool equality condition False == False" $ run [] (Eql (CstBool False) (CstBool False)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x == x" $ run [] (Eql (CstInt 1) (CstInt 1)) @?= Right (ValBool True),
    testCase "Testing ValInt equality condition x /= y" $ run [] (Eql (CstInt 1) (CstInt 2)) @?= Right (ValBool False),
    testCase "Testing equality on functions is an error" $ run [] (Eql (Lambda "" (CstBool True)) (Lambda "" (CstBool True))) @?= Left eqlErr,
    testCase "Testing equality type mismatch error btw int and bool" $ run [] (Eql (CstInt 1) (CstBool True)) @?= Left eqlErr,
    testCase "Testing equality type mismatch error btw function and int" $ run [] (Eql (Lambda "" (CstBool True)) (CstInt 1)) @?= Left eqlErr,
    testCase "Testing equality type mismatch error btw function and bool" $ run [] (Eql (Lambda "" (CstBool True)) (CstBool True)) @?= Left eqlErr,
    -- Test If (Exp)
    testCase "Testing conditional If ValInt error" $ run [] (If (CstInt 1) (CstInt 1) (CstInt 1)) @?= Left ifErr,
    testCase "Testing conditional If `error` error" $ run [] (If (Div (CstInt 1) (CstInt 0)) (CstInt 1) (CstInt 1)) @?= Left divByZeroErr,
    testCase "Testing conditional If `True`" $ run [] (If (CstBool True) (CstInt 3) (CstInt 1)) @?= Right (ValInt 3),
    testCase "Testing conditional If `False`" $ run [] (If (CstBool False) (CstInt 3) (CstInt 1)) @?= Right (ValInt 1),
    testCase "Testing conditional If ValFun error" $ 
      run [] (If (Lambda "" (CstBool True)) (CstInt 1) (CstInt 2)) @?= Left ifErr,
    testCase "Testing untaken branch not evaluated" $ run [] (If (CstBool True) (CstInt 1) (Div (CstInt 1) (CstInt 0))) @?= Right (ValInt 1)
  ]
evalEnvTests :: TestTree
evalEnvTests =
  testGroup
  "Testing evaluating on environments (?)"
  [
    -- Test eval env Var
    testCase "Testing lookup on empty environment error" $ run [] (Var "") @?= Left lookupErr,
    testCase "Testing failed lookup" $ run [("x", ValBool True)] (Var "y") @?= Left (lookupErr ++ "y"),
    testCase "Testing succesful lookup" $ run [("x", ValBool True)] (Var "x") @?= Right (ValBool True),
    -- Test eval env Let
    testCase "Let 'x'=3 evaluate x+x=6" $ run [] (Let ("x") (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
    testCase "Let 'x'=3 evaluate x+y=5 for y=2" $ run [("y", ValInt 2)] (Let ("x") (CstInt 3) (Add (Var "x") (Var "y"))) @?= Right (ValInt 5),
    testCase "Error when first expression errors" $
      run [] (Let ("x") (Pow (CstInt 1) (CstInt (-1))) (Add (Var "x") (Var "y"))) @?= Left negExpErr ,
    testCase "Let (shadowing)" $
      run
        []
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
    testCase "Example from handout" $ run [] (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i"))) @?= Right (ValInt 45),
    testCase "Bound must be integer" $ run [] (ForLoop ("p", CstInt 0) ("i", CstBool True) (Add (Var "p") (Var "i"))) @?= Left nonIntegErr,
    testCase "Can operate on bools" $ run [] (ForLoop ("bool", CstBool False) ("i", CstInt 11) (Eql (CstBool False) (Var "bool"))) @?= Right (ValBool True),
    testCase "Initial value error casts error" $ run [] (ForLoop("p", Var "missing") ("i", CstInt 10) (Add (Var "p") (Var "i"))) 
            @?= Left (lookupErr ++ "missing"),
    testCase "variable with name p already in env overwritten with initial" $ run [("p", ValInt 100)] (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
            @?= Right (ValInt 45),
    testCase "bound == 0 means returning initial value" $ run [] (ForLoop ("p", CstInt 100) ("i", CstInt 0) (Add (Var "p") (Var "i"))) @?= Right (ValInt 100), 
    testCase "body error propagates" $ run [] (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Div (Var "p") (Var "i"))) @?= Left divByZeroErr
  ]

evalFunTests :: TestTree
evalFunTests = 
  testGroup
  "Testing APL functions"
  [
    -- Test handout example
    testCase "Example from handout without apply" $ run [] (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) @?= 
        Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
    testCase "Example from handout with apply" $ run [] (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3)) @?= Right (ValInt 5),
    testCase "Test correct read of input env in nested ValFunc" $
      run [("x", ValInt 2)] (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 3)) @?= Right (ValInt 5),
    testCase "Test correct Apply error on first expression not evaluating to a ValFun" $ 
      run [] (Apply(Add(CstInt 2) (CstInt 3)) (CstInt 4)) @?= Left notValFunErr
    ]

evalTryCatchTests :: TestTree
evalTryCatchTests =
  testGroup
  "Testing try-catch"
  [
    -- Test try-cath in APL
    testCase "Correctly evaluate succesful expression" $
      run [] (TryCatch (Add (CstInt 1) (CstInt 2)) (CstBool False)) @?= Right (ValInt 3),
    testCase "Correctly return snd expression when fst is an error" $
      run [] (TryCatch (Div (CstInt 2) (CstInt 0)) (CstBool False)) @?= Right (ValBool False),
    testCase "Return second error when both expressions are errors" $
      run [] (TryCatch (Div (CstInt 2) (CstInt 0)) (Var "missing") ) 
            @?= Left (lookupErr ++ "missing")
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
      evalForLoopTests,
      evalFunTests,
      evalTryCatchTests
    ]
