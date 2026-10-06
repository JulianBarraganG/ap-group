module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, transactionTests, breakTestsPure, breakTestsIO, ioTests, localEnvTests]

-- Reads "x" from the environment, failing if it is unbound.
readX :: EvalM Val
readX = do
  env <- askEnv
  case envLookup "x" env of
    Just v -> pure v
    Nothing -> failure "x unbound"

bindX :: EvalM a -> EvalM a
bindX = localEnv (envExtend "x" (ValInt 1))

-- Each test fails with "x unbound" (or takes the wrong branch) if localEnv
-- does not reach into the nested computation of the effect.
localEnvTests :: TestTree
localEnvTests =
  testGroup
    "localEnv"
    [ testCase "TryCatchOp: reaches m1" $
        runEval (bindX $ catch readX (pure $ ValInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatchOp: reaches m2" $
        runEval (bindX $ catch (failure "boom") readX)
          @?= ([], Right (ValInt 1)),
      --
      testCase "TransactionOp: reaches payload" $
        runEval (bindX $ transaction readX)
          @?= ([], Right (ValInt 1)),
      --
      testCase "LoopOp: reaches body" $
        runEval (bindX $ looping readX)
          @?= ([], Right (ValInt 1)),
      --
      testCase "KvGetOp/KvPutOp: reaches continuation" $
        runEval (bindX $ evalKvPut (ValInt 0) (ValInt 2) >> evalKvGet (ValInt 0) >> readX)
          @?= ([], Right (ValInt 1))
    ]

breakTestsPure :: TestTree
breakTestsPure =
  testGroup
    "Break Pure"
      [ testCase "Pure: break returns from loop" $
          eval'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 100) $
                Let "_" (Break (CstBool True)) (Var "i")
            )
            @?= ([], Right (ValBool True)),
        --
        testCase "Pure: break outside loop" $
          eval' (Break (CstBool True))
            @?= ([], Left "Break outside loop"),
        --
        testCase "Pure: loop without break" $
          eval' (ForLoop ("p", CstInt 0) ("i", CstInt 3) (Add (Var "p") (Var "i")))
            @?= ([], Right (ValInt 3)),
        --
        -- p is 0, then 1; breaks at i = 2 with p = 1.
        testCase "Pure: break stops loop early" $
          eval'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                If (Eql (Var "i") (CstInt 2)) (Break (Var "p")) (Add (Var "p") (Var "i"))
            )
            @?= ([], Right (ValInt 1)),
        --
        -- Each inner loop breaks with 1; the outer loop runs all 3 iterations.
        testCase "Pure: break only exits innermost loop" $
          eval'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 3) $
                Add (Var "p") (ForLoop ("q", CstInt 0) ("j", CstInt 10) (Break (CstInt 1)))
            )
            @?= ([], Right (ValInt 3)),
        --
        testCase "Pure: break is not caught by TryCatch" $
          eval'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                TryCatch (Break (CstInt 7)) (CstInt 0)
            )
            @?= ([], Right (ValInt 7))
        --
 
        --
      ]
breakTestsIO :: TestTree
breakTestsIO =
  testGroup
    "Break IO"
      [ testCase "IO: break returns from loop" $ do
        res <-
          evalIO'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 100) $
                Let "_" (Break (CstBool True)) (Var "i")
            )
        res @?= Right (ValBool True),
      --
      testCase "IO: break outside loop" $ do
        res <- evalIO' (Break (CstBool True))
        res @?= Left "Break outside loop",
      --
      testCase "IO: loop without break" $ do
        res <- evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 3) (Add (Var "p") (Var "i")))
        res @?= Right (ValInt 3),
      --
      -- p is 0, then 1; breaks at i = 2 with p = 1.
      testCase "IO: break stops loop early" $ do
        res <-
          evalIO'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                If (Eql (Var "i") (CstInt 2)) (Break (Var "p")) (Add (Var "p") (Var "i"))
            )
        res @?= Right (ValInt 1),
      --
      testCase "IO: break is propagated by TryCatch inside loop" $ do
        res <-
          evalIO'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 10) $
                TryCatch (Break (CstInt 7)) (CstInt 0)
            )
        res @?= Right (ValInt 7),
      --
      -- Each inner loop breaks with 1; the outer loop runs all 3 iterations.
      -- If break exited all loops, the result would be 1 instead of 3.
      testCase "IO: break only exits innermost loop" $ do
        res <-
          evalIO'
            ( ForLoop ("p", CstInt 0) ("i", CstInt 3) $
                Add (Var "p") (ForLoop ("q", CstInt 0) ("j", CstInt 10) (Break (CstInt 1)))
            )
        res @?= Right (ValInt 3)
      ]

-- Examples from the Task 3 assignment text.
goodPut, badPut, get0, divByZero :: Exp
goodPut = KvPut (CstInt 0) (CstInt 1)
badPut = Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")
get0 = KvGet (CstInt 0)
divByZero = Div (CstInt 1) (CstInt 0)

transactionTests :: TestTree
transactionTests =
  testGroup
    "Transaction (assignment examples)"
    [ testCase "Pure: successful transaction is kept" $
        eval' (Let "_" (Transaction goodPut) get0)
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pure: failed transaction is rolled back" $
        eval' (TryCatch (Transaction badPut) get0)
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Pure: failed transaction propagates error" $
        eval' (Transaction badPut)
          @?= ([], Left "Unknown variable: die"),
      --
      testCase "Pure: prints kept on failure" $
        runEval (transaction (evalPrint "weee" >> failure "oh shit"))
          @?= (["weee"], Left "oh shit"),
      --
      testCase "Pure: nested, inner fails, outer kept" $
        eval'
          ( Let
              "_"
              (Transaction (Let "_" goodPut (TryCatch (Transaction badPut) (CstBool True))))
              get0
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pure: nested, both rolled back" $
        eval' (Let "_" (TryCatch (Transaction (Transaction badPut)) (CstBool True)) get0)
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "IO: successful transaction is kept" $ do
        res <- evalIO' (Let "_" (Transaction goodPut) get0)
        res @?= Right (ValInt 1),
      --
      testCase "IO: failed transaction propagates error" $ do
        res <- evalIO' (Transaction badPut)
        res @?= Left "Unknown variable: die",
      --
      testCase "IO: nested, inner fails, outer kept" $ do
        res <-
          evalIO'
            ( Let
                "_"
                (Transaction (Let "_" goodPut (TryCatch (Transaction badPut) (CstBool True))))
                get0
            )
        res @?= Right (ValInt 1),
      --
      -- Key 0 is missing after the rollback, so the interpreter prompts for it.
      testCase "IO: nested, both rolled back" $ do
        (_, res) <-
          captureIO ["ValInt 5"] $
            evalIO' (Let "_" (TryCatch (Transaction (Transaction badPut)) (CstBool True)) get0)
        res @?= Right (ValInt 5),
      --
      -- If the get read the wrong database it would prompt and return 9.
      testCase "IO: reads inside transaction see its own writes" $ do
        (_, res) <-
          captureIO ["ValInt 9"] $
            evalIO' (Transaction (Let "_" goodPut get0))
        res @?= Right (ValInt 1),
      --
      testCase "IO: transaction sees writes from before it" $ do
        (_, res) <-
          captureIO ["ValInt 9"] $
            evalIO' (Let "_" goodPut (Transaction get0))
        res @?= Right (ValInt 1)
    ]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),

      testCase "m2 never runs on successful m1" $
        eval' (TryCatch (CstInt 5) divByZero)
          @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatchOp (Free): failing m1 runs m2" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatchOp (Free): continuation receives the value" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) (\v -> pure $ ValBool (v == ValInt 1)))
          @?= ([], Right (ValBool True)),
      --
      testCase "KvPut then KvGet" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "KvGet missing key" $
        runEval (evalKvGet (ValInt 0))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
       testCase "KvPut overrides existing value" $
        eval' (Let "_" (Let "_" (KvPut (CstInt 0) (CstBool True)) (KvPut (CstInt 0) (CstBool False))) (KvGet (CstInt 0)))
          @?= ([], Right (ValBool False))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),
        testCase "print 2" $ do
           (out, res) <-
             captureIO [] $
               evalIO' $
                 Print "This is also 1" $
                   Print "This is 1" $
                     CstInt 1
           (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1),
        testCase "Double error" $ do
          res <- evalIO' (TryCatch ((CstInt 0) `Eql` (CstBool False)) ((CstInt 1) `Div` (CstInt 0)))
          res @?= Left "Division by zero",
        --
        testCase "TryCatchOp (Free): failing m1 runs m2" $ do
          res <- runEvalIO (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
          res @?= Right (ValInt 1),
        --
        testCase "TryCatchOp (Free): continuation receives the value" $ do
          res <- runEvalIO (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) (\v -> pure $ ValBool (v == ValInt 1)))
          res @?= Right (ValBool True),
        --
        testCase "m2 doesn't run on successful m1" $ do 
          res <- evalIO' (TryCatch (CstInt 5) divByZero)
          res @?= Right (ValInt 5),
        testCase "KvPut then KvGet" $ do
          res <-
            evalIO' $ Let "_" (KvPut (CstInt 0) (CstInt 1)) (KvGet (CstInt 0))
          res @?= Right (ValInt 1),
        --
        testCase "Missing key test" $ do
          (_, res) <-
            captureIO [" ValInt 1"] $ 
              runEvalIO $ 
                Free $ KvGetOp (ValInt 0) $ \val -> pure val
          res @?= Right (ValInt 1),
        --
        testCase "Missing key fail on bad prompt answer" $ do 
          (_, res) <- captureIO ["nono"] $ evalIO' $ KvGet(CstInt 0)
          res @?= Left "Invalid value input: nono",

        testCase "Entered keys don't get added to the database" $ do
          (_, res) <- captureIO ["ValBool True", "ValBool False"] $ evalIO' $ 
            Let "_" (KvGet (CstInt 0)) (KvGet (CstInt 0))
          res @?= Right (ValBool False),

        testCase "KvPut overrides existing value" $ do
          res <- evalIO' (Let "_" (Let "_" (KvPut (CstInt 0) (CstBool True)) (KvPut (CstInt 0) (CstBool False))) (KvGet (CstInt 0)))
          res @?= Right (ValBool False),

        testCase "Database is cleared after each evaluation" $ do 
          _ <- evalIO' $ KvPut (CstInt 0) (CstInt 1)
          (_, res2) <- captureIO ["ValBool True"] $ evalIO' (KvGet (CstInt 0))
          res2 @?= Right (ValBool True),
        --
        testCase "Transaction rolls back on failure" $ do
          res <-
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              _ <- catch (transaction (evalKvPut (ValInt 0) (ValInt 2) >> failure "oops")) (pure (ValInt 0))
              evalKvGet (ValInt 0)
          res @?= Right (ValInt 1)
    ]
