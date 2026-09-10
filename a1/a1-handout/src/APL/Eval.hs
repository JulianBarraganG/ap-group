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
  nonIntegErr,
  )
where

import APL.AST (Exp(..), VName)


-- Value
data Val = 
  ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
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
lookupErr :: Error
lookupErr = "Variable name not in environment: "
nonIntegErr :: Error
nonIntegErr = "Non-integral loop bound"
notValFunErr :: Error
notValFunErr = "First expression must evaluate to a ValFun"

-- Environment
type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend vname val env = (vname, val) : env

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
-- ENV OPERATIONS
eval env (Var vname) =
  case (envLookup vname env) of
    Nothing -> Left $ lookupErr ++ vname
    Just x -> Right x
eval env (Let vname e1 e2) =
  case (eval env e1) of
    Left err -> Left err
    Right val -> eval (envExtend vname val env) e2
-- FOR LOOPS 
eval env (ForLoop (p, initial) (i, bound) body) =
  case (eval env bound, eval env initial) of
    (Left err, _) -> Left err
    (Right (ValBool _), _) -> Left nonIntegErr
    (Right (ValFun _ _ _), _) -> Left nonIntegErr
    (_, Left err) -> Left err
    (Right (ValInt n), Right initVal) -> loop 0 initVal
      where
        -- counter: iterations done so far; acc: current value of p
        loop counter acc
          | counter >= n = Right acc
          | otherwise =
              case eval (envExtend i (ValInt counter) (envExtend p acc env)) body of
                Left err -> Left err
                Right acc' -> loop (counter + 1) acc'
-- FUNCTION EVALUATION
eval env (Lambda vname e1) = Right $ ValFun env vname e1
eval env (Apply e1 e2) =
  case(eval env e1, eval env e2) of 
    (Right (ValFun env' vname body), Right argVal) -> eval (envExtend vname argVal env') body
    (Right (ValBool _), _) -> Left notValFunErr 
    (Right (ValInt _), _) -> Left notValFunErr
    (Left err, _) -> Left err
    (_, Left err) -> Left err
