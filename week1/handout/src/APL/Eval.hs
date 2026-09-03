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
data Val = 
  ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

-- Error Messages
type Error = String
divByZeroErr :: Error
divByZeroErr = "Division by zero error"
negExpErr :: Error
negExpErr = "Negative exponent error"
arithBoolErr :: Error
arithBoolErr = "Arithmetics only defined for Integers"
eqlErr :: Error
eqlErr = "Eql must compare same type class e.g. ValBool"
ifErr :: Error
ifErr = "Condition must be type ValBool, not ValInt"


-- Eval function to evaluate expressions into values
eval :: Exp -> Either Error Val
-- CONSTRUCTORS
eval (CnstInt x) = Right $ ValInt x
eval (CnstBool x) = Right $ ValBool x
-- ARITHMETICS
-- Addition
eval (Add e1 e2) = 
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x+y
    (Right _, Right _) -> Left arithBoolErr
-- Subtraction
eval (Sub e1 e2) = 
  case(eval e1, eval e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x-y
    (Right _, Right _) -> Left arithBoolErr
-- Multiplication
eval (Mul e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x*y
    (Right _, Right _) -> Left arithBoolErr
-- Division (integer)
eval (Div e1 e2) = 
  case(eval e1, eval e2) of 
    (_, Right(ValInt 0)) -> Left $ divByZeroErr
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y)) -> Right $ ValInt $ x `div` y
    (Right _, Right _) -> Left arithBoolErr
-- Power (integer)
eval (Pow e1 e2) =
  case(eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y))
      | y < 0 -> Left $ negExpErr
      | otherwise -> Right $ ValInt $ x^y
    (Right _, Right _) -> Left arithBoolErr
-- CONDITIONS
-- Equality for Expressions
eval (Eql e1 e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValBool _)) -> Left $ eqlErr
    (Right (ValBool _), Right (ValInt _)) -> Left $ eqlErr
    (Right x, Right y) -> Right $ ValBool $ x == y
-- If
eval (If e1 e2 e3) =
  case eval e1 of
    Left err -> Left err
    Right (ValInt _) -> Left ifErr
    Right (ValBool b)
      | b -> eval e2
      | otherwise -> eval e3
