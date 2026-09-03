module APL.Eval
  (
  Val(..),
  -- Functions
  eval,
  envEmpty,
  -- Errors
  lookupErr,
  divByZeroErr,
  negExpErr,
  eqlErr,
  ifErr,
  )
where

import APL.AST (Exp(..), VName)


-- Value
data Val = 
  ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

-- Environment
type Env = [(VName, Val)]

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
lookupErr :: Error
lookupErr = "Variable name not in environment: "

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend vname val env = [(vname, val)] ++ env

-- | Look up a variable by name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val -- Nothing | Val
envLookup vname env =
  case (vname, env) of
    (_, []) -> Nothing
    (x, ((en, ev) : es))
      | x == en -> (Just ev)
      | otherwise -> envLookup x es

-- Eval function to evaluate expressions into values
eval :: Env -> Exp -> Either Error Val
-- CONSTRUCTORS
eval _ (CstInt x) = Right $ ValInt x
eval _ (CstBool x) = Right $ ValBool x
--ARITHMETICS
-- Addition
eval env (Add e1 e2) = 
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x+y
    (Right _, Right _) -> Left arithBoolErr
-- Subtraction
eval env (Sub e1 e2) = 
  case(eval env e1, eval env e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x-y
    (Right _, Right _) -> Left arithBoolErr
-- Multiplication
eval env (Mul e1 e2) =
  case(eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x*y
    (Right _, Right _) -> Left arithBoolErr
-- Division (integer)
eval env (Div e1 e2) = 
  case(eval env e1, eval env e2) of 
    (_, Right(ValInt 0)) -> Left $ divByZeroErr
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y)) -> Right $ ValInt $ x `div` y
    (Right _, Right _) -> Left arithBoolErr
-- Power (integer)
eval env (Pow e1 e2) =
  case(eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right(ValInt x), Right(ValInt y))
      | y < 0 -> Left $ negExpErr
      | otherwise -> Right $ ValInt $ x^y
    (Right _, Right _) -> Left arithBoolErr
-- CONDITIONS
-- Equality for Expressions
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValBool _)) -> Left $ eqlErr
    (Right (ValBool _), Right (ValInt _)) -> Left $ eqlErr
    (Right x, Right y) -> Right $ ValBool $ x == y
-- If
eval env (If e1 e2 e3) =
  case eval env e1 of
    Left err -> Left err
    Right (ValInt _) -> Left ifErr
    Right (ValBool b)
      | b -> eval env e2
      | otherwise -> eval env e3
eval env (Var vname) =
  case (envLookup vname env) of
    Nothing -> Left $ lookupErr ++ vname
    Just x -> Right x
eval env (Let vname e1 e2) =
  case (eval env e1) of
    Left err -> Left err
    Right val -> eval (envExtend vname val env) e2
