module APL.Eval
  (
  Val(..),
  eval,
  divByZeroErr,
  negExpErr,
  )
where

import APL.AST (Exp(..))


-- Value
data Val = ValInt Integer
  deriving (Eq, Show)

-- Error Messages
type Error = String
divByZeroErr :: Error
divByZeroErr = "Division by zero error"
negExpErr :: Error
negExpErr = "Negative exponent error"


-- Eval function to evaluate expressions into values
eval :: Exp -> Either Error Val
-- Constructor
eval (CnstInt x) = Right $ ValInt x
-- Addition
eval (Add e1 e2) = 
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x+y
-- Subtraction
eval (Sub e1 e2) = 
  case(eval e1, eval e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x-y
-- Multiplication
eval (Mul e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x*y
-- Division (integer)
eval (Div e1 e2) = 
  case(eval e1, eval e2) of 
    (_, Right(ValInt 0)) -> Left $ divByZeroErr
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y)) -> Right $ ValInt $ x `div` y
-- Power (integer)
eval (Pow e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y))
      | y < 0 -> Left $ negExpErr
      | otherwise -> Right $ ValInt $ x^y
