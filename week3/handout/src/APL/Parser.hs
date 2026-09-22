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
    -- parseTest,
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
    

keywords :: [String]
keywords = ["true", "false", "if"]

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  void $ chunk s
  notFollowedBy (satisfy isAlpha)

pBool :: Parser Bool
pBool = 
  choice
    [ lKeyword "true" >> pure True,
      lKeyword "false" >> pure False
    ]

pExp :: Parser Exp
pExp = 
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName
    ]

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
