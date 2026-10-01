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
tests = testGroup "Free monad interpreters" [pureTests, ioTests, transactionTests]

-- Examples from the Task 3 assignment text.
goodPut, badPut, get0 :: Exp
goodPut = KvPut (CstInt 0) (CstInt 1)
badPut = Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")
get0 = KvGet (CstInt 0)

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
          @?= ([], Left "Key not in state"),
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
          @?= ([], Left "Key not in state"),
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
        res @?= Right (ValInt 5)
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
      testCase "Error strings concatenate" $
        runEval (catch (evalPrint "a" >> failure "x") (evalPrint "b" >> pure (ValInt 1)))
        @?= (["a", "b"], Right (ValInt 1)),
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
          @?= ([], Left "Key not in state"),
      --
      testCase "Transaction keeps state on success" $
        runEval
          ( do
              _ <- transaction (evalKvPut (ValInt 0) (ValInt 1) >> pure (ValInt 1))
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction rolls back on failure" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              _ <- catch (transaction (evalKvPut (ValInt 0) (ValInt 2) >> failure "oops")) (pure (ValInt 0))
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction propagates error" $
        runEval (transaction (failure "oops"))
          @?= ([], Left "oops"),
      --
      testCase "Transaction keeps prints on failure" $
        runEval (transaction (evalPrint "weee" >> failure "oh no"))
          @?= (["weee"], Left "oh no")
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
          res <- runEvalIO (eval (TryCatch ((CstInt 0) `Eql` (CstBool False)) ((CstInt 1) `Div` (CstInt 0))))
          res @?= Left "Division by zero",
        --
        testCase "KvPut then KvGet" $ do
          res <-
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvGet (ValInt 0)
          res @?= Right (ValInt 1),
        --
        testCase "Missing key test" $ do
          (_, res) <-
            captureIO [" ValInt 1"] $ 
              runEvalIO $ 
                Free $ KvGetOp (ValInt 0) $ \val -> pure val
          res @?= Right (ValInt 1),
        --
        testCase "Transaction keeps state on success" $ do
          res <-
            runEvalIO $ do
              _ <- transaction (evalKvPut (ValInt 0) (ValInt 1) >> pure (ValInt 1))
              evalKvGet (ValInt 0)
          res @?= Right (ValInt 1),
        --
        testCase "Transaction rolls back on failure" $ do
          res <-
            runEvalIO $ do
              evalKvPut (ValInt 0) (ValInt 1)
              _ <- catch (transaction (evalKvPut (ValInt 0) (ValInt 2) >> failure "oops")) (pure (ValInt 0))
              evalKvGet (ValInt 0)
          res @?= Right (ValInt 1),
        --
        testCase "Transaction propagates error" $ do
          res <- runEvalIO (transaction (failure "oops"))
          res @?= Left "oops",
        --
        testCase "Nested transaction rolls back inner only" $ do
          res <-
            runEvalIO $ do
              _ <-
                transaction $ do
                  evalKvPut (ValInt 0) (ValInt 1)
                  catch (transaction (evalKvPut (ValInt 0) (ValInt 2) >> failure "oops")) (pure (ValInt 0))
              evalKvGet (ValInt 0)
          res @?= Right (ValInt 1)
    ]
