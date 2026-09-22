module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

keywordErr :: String
keywordErr = "Unexpected keyword"

lexeme :: Parser a -> Parser a
lexeme p = p <* space

lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail keywordErr
    else pure v
    
infixOps :: [String]
infixOps = ["+", "-", "*", "/"]

keywords :: [String]
keywords = ["true", "false", "if", "then", "else"] ++ infixOps


lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lInfix :: String -> Parser ()
lInfix s = lexeme $ void $ try $ chunk s

pBool :: Parser Bool
pBool = 
  choice
    [ lKeyword "true" >> pure True,
      lKeyword "false" >> pure False
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lInfix "(" *> pExp <* lInfix ")"
    ]

pLExp :: Parser Exp
pLExp =
  choice
  [ If
      <$> (lKeyword "if" *> pExp0)
      <*> (lKeyword "then" *> pExp0)
      <*> (lKeyword "else" *> pExp0),
    pAtom
  ]

pExp0 :: Parser Exp
pExp0 = do
  x <- pExp1
  chain x
  where
    chain x = 
      choice
        [ do
            lInfix "+"
            y <- pExp1
            chain $ Add x y,
          do
            lInfix "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = do
  x <- pLExp
  chain x
  where
    chain x =
      choice
        [ do
            lInfix "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lInfix "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
