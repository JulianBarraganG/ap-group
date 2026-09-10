module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

prettyPrintExp :: TestTree
prettyPrintExp =
  testGroup
  "Testing pretty printing of expressions in APL"
  [
    -- Test printExp in APL
    testCase "Testing (CstBool True) prints 'true'" $
      printExp (CstBool True) @?= "true",
    testCase "Testing (CstBool False) prints 'false'" $
      printExp (CstBool False) @?= "false",
    testCase "Print base case integet as decimal string" $
      printExp (CstInt 11) @?= "11"
  ]

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      prettyPrintExp
    ]
