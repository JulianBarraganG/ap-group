module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    -- Errors
    divByZeroErr,
    negExpErr,
    arithNonIntErr,
    eqlErr,
    ifErr,
    lookupErr,
    nonIntegErr,
    notValFunErr,
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
divByZeroErr :: Error
divByZeroErr = "Division by zero error"
negExpErr :: Error
negExpErr = "Negative exponent error"
arithNonIntErr :: Error
arithNonIntErr = "Arithmetics only defined for Integers"
eqlErr :: Error
eqlErr = "Eql must compare same type class and either ValBool or ValInt"
ifErr :: Error
ifErr = "Condition must be type ValBool, not ValInt"
lookupErr :: Error
lookupErr = "Variable name not in environment: "
nonIntegErr :: Error
nonIntegErr = "Non-integral loop bound"
notValFunErr :: Error
notValFunErr = "First expression must evaluate to a ValFun"

newtype EvalM a = EvalM (Either Error a)

instance Functor EvalM where
  fmap _ (EvalM (Left e)) = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right $ f x

instance Applicative EvalM where
  pure x =  EvalM $ Right x 
  _ <*> EvalM (Left e) = EvalM $ Left e
  EvalM (Left e) <*> _ = EvalM $ Left e 
  EvalM (Right f) <*> EvalM (Right x) = EvalM $ Right $ f x

instance Monad EvalM where
  EvalM (Left e) >>= _ = EvalM $ Left e
  EvalM (Right x) >>= f = f x



runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x

failure :: String -> EvalM a
failure s = EvalM $ Left s

eval :: Env -> Exp -> EvalM Val

-- BASE CASES

eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool x) = pure $ ValBool x

-- ARITHMETICS 

eval env (Add e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' + y')
    _ -> failure arithNonIntErr

eval env (Sub e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' - y')
    _ -> failure arithNonIntErr

eval env (Mul e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure $ ValInt (x' * y')
    _ -> failure arithNonIntErr

eval env (Div e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (_, ValInt 0) -> failure divByZeroErr 
    (ValInt x', ValInt y') -> pure $ ValInt (x' `div` y')
    _ -> failure arithNonIntErr

eval env (Pow e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y')
      | y' < 0 -> failure negExpErr
      | otherwise -> pure $ ValInt (x' ^ y')
    _ -> failure arithNonIntErr

-- CONDITIONS 

eval env (Eql e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x,y) of
    (ValInt x', ValInt y') -> pure $ ValBool (x' == y')
    (ValBool x', ValBool y') -> pure $ ValBool (x' == y')
    _ -> failure eqlErr

eval env (If e1 e2 e3) = do
  x <- eval env e1
  case x of
    ValBool True -> eval env e2
    ValBool False -> eval env e3
    _ -> failure ifErr

-- ENVIRONMENT OPERATIONS 

eval env (Var vname) =
  case envLookup vname env of
    Just x -> pure x
    Nothing -> failure (lookupErr ++ vname)


eval env (Let vname e1 e2) = do
  x <- eval env e1
  eval (envExtend vname x env) e2


-- MISC 
eval env (ForLoop (p, initial) (i, bound) body) = do
  v <- eval env initial
  n <- eval env bound
  case n of
    ValInt n' -> loop 0 v 
      where 
        loop counter acc
          | counter < n' = do
            acc' <- eval (envExtend i (ValInt counter) (envExtend p acc env)) body
            loop (counter + 1) acc'
          | otherwise = pure acc
    _ -> failure nonIntegErr
 
eval env (Lambda vname e1) = pure $ ValFun env vname e1

eval env (Apply e1 e2) = do
  fun <- eval env e1
  case fun of
    ValFun env' v body -> do 
      argVal <- eval env' e2
      eval (envExtend v argVal env') body
    _ -> failure notValFunErr

eval _ _ = undefined
