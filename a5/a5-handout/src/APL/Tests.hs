module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp, Vars)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , sized
  , withMaxSuccess
  , frequency
  , elements
  , suchThat
  , choose
  , vectorOf
  , tabulate
  , oneof
  , sample
  , quickCheck
  )
import Text.Megaparsec (runParser)
import APL.Parser (parseAPL)

keywords :: [String]
keywords = [
            "try", 
            "catch",
            "if",
            "else",
            "then",
            "let", 
            "do",
            "print",
            "put",
            "get",
            "for",
            "in",
            "loop"
            ]

alphaChars :: String
alphaChars = ['a'..'z'] ++ ['A'..'Z']
alphaNumChars :: String
alphaNumChars = alphaChars ++ ['0'..'9']

genVName :: Gen VName
genVName = genName `suchThat` (`notElem` keywords)
  where 
    genName = do
      c <- elements alphaChars
      n <- choose (1, 3)
      cs <- vectorOf n (elements alphaNumChars)
      pure (c : cs)

instance Arbitrary Exp where
  arbitrary = sized $ genExp []

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: Vars -> Int -> Gen Exp
genExp vars 0 = frequency [
  (4, CstInt <$> arbitrary),
  (1, CstBool <$> arbitrary),
  (if null vars then 0 else 3,
      if null vars
      then Var <$> genVName
      else Var <$> elements vars
  )]

genExp vars size =
  frequency
    [ (1, CstInt <$> arbitrary),
      (1, CstBool <$> arbitrary),
      (if null vars then 1 else 3,
      if null vars
      then Var <$> genVName
      else Var <$> elements vars
      ),
      (1, Add <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, Sub <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, Mul <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, Div <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, Pow <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, Eql <$> genExp vars halfSize <*> genExp vars halfSize),
      (1, If <$> genExp vars thirdSize <*> genExp vars thirdSize <*> genExp vars thirdSize),
      (6, do 
            name <- genVName
            Let name <$> genExp vars halfSize <*> genExp (name : vars) halfSize
      ),
      (6, do
            name <- genVName
            Lambda name <$> genExp (name : vars) (size - 1)
      ),
      (1, Apply <$> genExp vars halfSize <*> genExp vars halfSize ),
      (1, TryCatch <$> genExp vars halfSize <*> genExp vars halfSize)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3


expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e = 
  case (parseAPL "" (printExp e)) of
    Right e' -> e' == e
    Left _ -> False

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
