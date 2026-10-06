module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (
  Gen,
  listOf,
  elements,
  suchThat,
  oneof,
  Arbitrary (arbitrary),
  sample,
  )

keywords :: [String]
keywords = [
            "try", 
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
      cs <- listOf (elements alphaNumChars)
      pure (c : cs)

genInt :: Gen Exp
genInt = CstInt <$> arbitrary 

genBool :: Gen Exp
genBool = CstBool <$> arbitrary

genAdd :: Int -> Gen Exp
genAdd n = Add <$> genExp n <*> genExp n

genSub :: Int -> Gen Exp
genSub n = Sub <$> genExp n <*> genExp n

genDiv :: Int -> Gen Exp
genDiv n = Div <$> genExp n <*> genExp n

genMul :: Int -> Gen Exp
genMul n = Mul <$> genExp n <*> genExp n

genPow :: Int -> Gen Exp
genPow n = Pow <$> genExp n <*> genExp n

genEql :: Int -> Gen Exp
genEql n = Eql <$> genExp n <*> genExp n

genIf :: Int -> Gen Exp
genIf n = If <$> genExp n <*> genExp n <*> genExp n

genTryCatch :: Int -> Gen Exp
genTryCatch n = TryCatch <$> genExp n <*> genExp n

genLet :: Int -> Gen Exp
genLet n = Let <$> genVName <*> genExp n <*> genExp n

genVar :: Gen Exp
genVar = Var <$> genVName

genLambda :: Int -> Gen Exp
genLambda n = Lambda <$> genVName <*> genExp n

genApply :: Int -> Gen Exp
genApply n = Apply <$> genExp n <*> genExp n
  
genExp :: Int -> Gen Exp
genExp size = 
  if size <= 1 then oneof [genVar, genInt, genBool]
  else if size == 2 then oneof [genVar, genInt, genBool, genLambda (size - 1)]
  else 
    let half = (size - 1) `div` 2 in
      oneof 
        [ genVar,
          genInt,
          genBool,
          genLambda (size - 1),
          genAdd half,
          genSub half,
          genDiv half,
          genMul half,
          genPow half,
          genEql half,
          genLet half,
          genApply half,
          genTryCatch half,
          genIf ((size - 1) `div` 3)
        ]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc = undefined

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc = undefined
