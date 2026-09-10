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
    -- 'Base Cases' bools and ints (currently)
    testCase "Testing (CstBool True) prints 'true'" $
      printExp (CstBool True) @?= "true",
    testCase "Testing (CstBool False) prints 'false'" $
      printExp (CstBool False) @?= "false",
    testCase "Print base case integer as decimal string" $
      printExp (CstInt 11) @?= "11",
    testCase "Addition prints (x + y)" $
    -- Binary Operations (arithmetics)
      printExp (Add (CstInt 1) (CstInt 2)) @?= "(1 + 2)",
    testCase "Subtraction prints (x - y)" $
      printExp (Sub (CstInt 1) (CstInt 2)) @?= "(1 - 2)",
    testCase "Division prints (x / y)" $
      printExp (Div (CstInt 1) (CstInt 2)) @?= "(1 / 2)",
    testCase "Multiplication prints (x * y)" $
      printExp (Mul (CstInt 1) (CstInt 2)) @?= "(1 * 2)",
    testCase "Raising x to power of y prints (x ** y)" $
      printExp (Pow (CstInt 1) (CstInt 2)) @?= "(1 ** 2)",
    -- Function Expressions
    testCase "Lambda function (\\x -> y) succes test" $
      printExp (Lambda "x" (CstInt 2)) @?= "(\\x -> 2)",
    testCase "Apply prints '(x y)'" $
      printExp (Apply (CstBool True) (CstBool False)) @?= "(true false)",
    testCase "For loop prints '(loop x = val for i < bound do body)'" $
      printExp (ForLoop ("acc", CstInt 0) ("i", CstInt 3) (Add (Var "acc") (Var "i")))
      @?= "(loop acc = 0 for i < 3 do (acc + i))",
    testCase "Try-catch prints '(try exp1 catch exp2)'" $
      printExp (TryCatch (CstBool True) (CstBool False))
      @?= "(try true catch false)",
    -- Conditionals
    testCase "If prints '(if x then y else z)'" $
      printExp (If (CstBool True) (Var "yay") (Var "nay")) @?= "(if true then yay else nay)",
    testCase "Eql prints '(x == y)'" $
      printExp (Eql (Var "x") (Var "y")) @?= "(x == y)",
    -- Environment stuff
    testCase "Var prints the variable name" $ printExp (Var "x") @?= "x",
    testCase "Let prints '(let x in y)'" $
      printExp (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= "(let x = 3 in (x + y))",
    -- Some nested ones
    testCase "Gaussian formula test" $
    printExp (
      Add (Pow (Var "e") (Mul (Var "i") (Var "pi"))) (CstInt 1)
    )
    @?= "((e ** (i * pi)) + 1)"
  ]

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      prettyPrintExp
    ]
