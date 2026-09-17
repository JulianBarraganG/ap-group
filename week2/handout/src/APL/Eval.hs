module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    EvalM,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap f (EvalM x) = 
    EvalM $ \env -> case x env of
      Right x' -> Right $ f x'
      Left err -> Left err

instance Applicative EvalM where
  pure a = EvalM $ \_env -> Right a
  EvalM ef <*> EvalM ex = EvalM $ \env -> 
    case (ef env, ex env) of
      (Right f, Right x) -> Right (f x)
      _ -> Left "Applicative Error"

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' -> let EvalM y = f x'
         in y env


failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Right x -> Right x
    Left _ ->  m2 env

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv envF (EvalM a) = EvalM $ \env -> a (envF env)

runEval :: EvalM a -> Either Error a
runEval (EvalM a) = a envEmpty

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x
eval (Add e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n + m
    _ -> failure "Non-integer operand"
eval (Sub e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n - m
    _ -> failure "Non-integer operand"
eval (Mul e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n * m
    _ -> failure "Non-integer operand"
eval (Div e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt _, ValInt 0) -> failure "Div-by zero error"
    (ValInt n, ValInt m) -> pure $ ValInt $ n `div` m
    _ -> failure "Non-integer operand"
eval (Pow e1 e2) = do
  x <- eval e1
  y <- eval e2
  case y of
    (ValInt m)
      | m < 0 -> failure "Exponent must be positive"
      | otherwise -> case x of
        (ValInt n) -> pure $ ValInt $ n ^ m
        _ -> failure "Non-integer operand"
    _ -> failure "Non-integer operand"
-- CONDITIONS
eval (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValBool $ n == m
    (ValBool n, ValBool m) -> pure $ ValBool $ n == m
    (_, _) -> failure "Equality not defined for given types"
eval (If e1 e2 e3) = do
  x <- eval e1
  case x of
    (ValBool True) -> eval e2
    (ValBool False) -> eval e3
    _ -> failure "If condition must be ValBool"
-- ENVIRONMENT STUFF
eval (Var e1) = 
  askEnv >>= \env ->
  case (envLookup e1 env) of
    Just x -> pure x
    Nothing -> failure "Failed to find value for var"
eval (Let vname e1 e2) = do -- let x = e1 in e2
  x <- eval e1
  localEnv (envExtend vname x) (eval e2)
-- FUNCTIONS
eval (ForLoop (p, initial) (i, bound) body) = do
  v <- eval initial
  n <- eval bound
  case n of
    (ValInt n') -> loop 0 v
      where 
      loop counter acc
        | counter >= n' = pure acc
        | otherwise = do
          acc' <- localEnv (envExtend p acc . envExtend i (ValInt counter)) (eval body)
          loop (counter + 1) acc'
    _ -> failure "Bound must be integer"
eval (Lambda vname e1) = do
  env <- askEnv
  pure $ ValFun env vname e1
eval (Apply e1 e2) = do
  vFun <- eval e1
  case vFun of
    (ValFun _ vname body) -> do 
      argVal <- eval e2
      localEnv (envExtend vname argVal) (eval body)
    (_) -> failure "Exp 1 must evaluate to ValFun"

eval (TryCatch e1 e2) = catch (eval e1) (eval e2)
