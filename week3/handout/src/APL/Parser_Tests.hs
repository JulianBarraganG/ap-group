module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Test that some string parses to the provided Exp.
parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

-- Test that some string results in a parse error. The exact error
-- message is not tested. Be careful when using this function, as it
-- is easy to make a test that fails for some reason (such as a typo)
-- than the case you are actually interested in. Generally, negative
-- tests of parsers are often not very interesting.
parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

baseParserTests :: TestTree
baseParserTests =
  testGroup
    "Basic parser tests"
    [
      parserTest "2" (CstInt 2),
      parserTest "23" (CstInt 23),
      parserTestFail "2w",
      parserTest "2   " (CstInt 2), 
      parserTest "     1" (CstInt 1),
      parserTest "ab1" (Var "ab1"),
      parserTestFail "if",
      parserTest "true" (CstBool True),
      parserTest "false" (CstBool False),
      parserTestFail "falsexx"
    ]

leftRecursionTests :: TestTree
leftRecursionTests =
  testGroup
    "Left recursion tests"
      [
        parserTest "x" (Var "x"),
        parserTest "x + y" (Add (Var "x") (Var "y")),
        parserTest "x+y" (Add (Var "x") (Var "y")),
        parserTest "x-y" (Sub (Var "x") (Var "y")),
        parserTest "x*y" (Mul (Var "x") (Var "y")),
        parserTest "x/y" (Div (Var "x") (Var "y")),
        parserTest "x+y*z" (Add (Var "x") (Mul (Var "y") (Var "z"))),
        parserTest "x+(y*z)" (Add (Var "x") (Mul (Var "y") (Var "z"))),
        parserTest "if x then x else x + x" (If (Var "x") (Var "x") (Add (Var "x") (Var "x")))
      ]

tests :: TestTree
tests = testGroup "Parsing" [baseParserTests, leftRecursionTests]
