module APL.Eval
  (
  Val(..),
  eval,
  )
where

import APL.AST (Exp(..))

data Val = ValInt Integer
  deriving (Eq, Show)


type Error = String


eval :: Exp -> Either Error Val

eval (CnstInt x) = Right $ ValInt x

eval (Add e1 e2) = 
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x+y

eval (Sub e1 e2) = 
  case(eval e1, eval e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x-y

eval (Mul e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x*y

eval (Div e1 e2) = 
  case(eval e1, eval e2) of 
    (_, Right(ValInt 0)) -> Left $ "Division by zero error"
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y)) -> Right $ ValInt $ x `div` y

eval (Pow e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y))
      | y < 0 -> Left $ "Negative exponent"
      | otherwise -> Right $ ValInt $ x^y
