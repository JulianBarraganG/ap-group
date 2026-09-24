module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x==y" $ Eql (Var "x") (Var "y"),
          parserTest "x**y" $ Pow (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x==x+y" $ Eql (Var "x") (Add (Var "x") (Var "y")),
          parserTest "x**x+y" $ Add (Pow (Var "x") (Var "x")) (Var "y"),
          parserTest "x+x**y" $ Add (Var "x") (Pow (Var "x") (Var "y"))
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Function Expressions (LExp)"
        [ parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try (x / y) catch z" $ TryCatch (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "let x = x in x" $ Let "x" (Var "x") (Var "x"),
          parserTest "let x = 1 in (x + y)" $ Let "x" (CstInt 1) (Add (Var "x") (Var "y")),
          parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          parserTest "\\x -> x + x" $ Lambda "x" (Add (Var "x") (Var "x")),
          parserTest "loop i = 1 for p < 10 do y" $ ForLoop ("i", CstInt 1) ("p", CstInt 10) (Var "y")
        ],
      testGroup
        "Putting, printing and getting"
        [ parserTest "get x" $ KvGet (Var "x"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "get (x + y)" $ KvGet (Add (Var "x") (Var "y")),
          parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "put x y + z" $ Add (KvPut (Var "x") (Var "y")) (Var "z"),
          parserTest "put x (y + z)" $ KvPut (Var "x") (Add (Var "y") (Var "z")),
          parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"foo has a value\" x" $ Print "foo has a value" (Var "x"), -- alphanum w/ spaces
          parserTest "print \"@.foo-=\bbar!_?\" x" $ Print "@.foo-=\bbar!_?" (Var "x"), -- alphanum w/ special chars
          parserTest "print \"put (foo + bar)\" x" $ Print "put (foo + bar)" (Var "x") -- operators
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ]
    ]
