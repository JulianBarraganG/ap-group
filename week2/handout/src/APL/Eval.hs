module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
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

newtype EvalM a = EvalM (Either Error a) deriving Show

instance Functor EvalM where
  fmap _ (EvalM (Left err)) = EvalM $ Left err
  fmap f (EvalM (Right a)) = EvalM $ Right (f a)

instance Applicative EvalM where
  pure a = EvalM $ Right a
  (EvalM (Right f)) <*> (EvalM (Right a)) = EvalM $ Right (f a)
  -- _ <*> _ = EvalM $ Left "Applicative Error"
  (EvalM (Left err1)) <*> (EvalM (Right _)) = EvalM $ Left err1
  (EvalM (Right _)) <*> (EvalM (Left err2)) = EvalM $ Left err2
  (EvalM (Left err1)) <*> (EvalM (Left err2)) = EvalM $ Left ( err1 ++ err2)

instance Monad EvalM where
  EvalM (Left err) >>= _ = EvalM $ Left err
  EvalM (Right a) >>= f =  f a 

runEval :: EvalM a -> Either Error a
runEval (EvalM a) = a

failure :: String -> EvalM a
failure s = EvalM $ Left s

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x -- EvalM $ Right (ValInt x)
eval _ (CstBool x) = pure $ ValBool x -- EvalM $ Right (ValBool x)
eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n + m
    _ -> failure "Non-integer operand"
eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n - m
    _ -> failure "Non-integer operand"
eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValInt $ n * m
    _ -> failure "Non-integer operand"
eval env (Div e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt _, ValInt 0) -> failure "Div-by zero error"
    (ValInt n, ValInt m) -> pure $ ValInt $ n `div` m
    _ -> failure "Non-integer operand"
eval env (Pow e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case y of
    (ValInt m)
      | m < 0 -> failure "Exponent must be positive"
      | otherwise -> case x of
        (ValInt n) -> pure $ ValInt $ n ^ m
        _ -> failure "Non-integer operand"
    _ -> failure "Non-integer operand"
-- CONDITIONS
eval env (Eql e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt n, ValInt m) -> pure $ ValBool $ n == m
    (ValBool n, ValBool m) -> pure $ ValBool $ n == m
    (_, _) -> failure "Equality not defined for given types"
eval env (If e1 e2 e3) = do
  x <- eval env e1
  case x of
    (ValBool True) -> eval env e2
    (ValBool False) -> eval env e3
    _ -> failure "If condition must be ValBool"
-- ENVIRONMENT STUFF
eval env (Var e1) = 
  case (envLookup e1 env) of
    Just x -> pure x
    Nothing -> failure "Failed to find value for var"
eval env (Let vname e1 e2) = do -- let x = e1 in e2
  x <- eval env e1
  eval (envExtend vname x env) e2
-- FUNCTIONS
eval env (ForLoop (p, initial) (i, bound) body) = do
  v <- eval env initial
  n <- eval env bound
  case n of
    (ValInt n') -> loop 0 v
      where 
      loop counter acc
        | counter >= n' = pure acc
        | otherwise = do
          acc' <- (eval (envExtend p acc (envExtend i (ValInt counter) env)) body)
          loop (counter + 1) acc'
    _ -> failure "Bound must be integer"
eval env (Lambda vname e1) = pure $ ValFun env vname e1
eval env (Apply e1 e2) = do
  vFun <- eval env e1
  case vFun of
    (ValFun env' vname' e1') -> do 
      argVal <- eval env' e2
      eval (envExtend vname' argVal env') e1'
    (_) -> failure "Exp 1 must evaluate to ValFun"
