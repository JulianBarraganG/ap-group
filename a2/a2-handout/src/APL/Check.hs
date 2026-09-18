module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

import Control.Monad (ap, liftM)

type Error = String
type EnvVars = [VName]

newtype CheckM a = CheckM (EnvVars -> Either Error a) -- TODO - give this a proper definition.

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \_env -> Right x
  (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      Left x' -> Left x'
      Right x' -> 
        let CheckM y = f x' in y env

askVars :: CheckM EnvVars
askVars = CheckM $ \vars -> Right vars

noVars :: VName -> CheckM a
noVars var = CheckM $ \_vars -> Left ("Variable not in scope: " ++ var)


varsExtend :: VName -> EnvVars -> EnvVars
varsExtend v env = v : env

localVars :: (EnvVars -> EnvVars) -> CheckM a -> CheckM a
localVars f (CheckM m) = CheckM $ \envVars -> m (f envVars)


simpleCheckHelper :: Exp -> Exp -> CheckM ()
simpleCheckHelper e1 e2 = do
  _ <- check e1
  _ <- check e2
  pure ()

check :: Exp -> CheckM ()
check (CstInt _) = pure ()

check (CstBool _) = pure ()

check (Var v) = do
  vars <- askVars
  if (elem v vars) then pure() else (noVars v)

check (Add e1 e2) = 
  simpleCheckHelper e1 e2

check (Mul e1 e2) =  
  simpleCheckHelper e1 e2

check (Sub e1 e2) = 
  simpleCheckHelper e1 e2

check (Div e1 e2) = 
  simpleCheckHelper e1 e2

check (Pow e1 e2) = 
  simpleCheckHelper e1 e2

check (Eql e1 e2) =
 simpleCheckHelper e1 e2

check (If e1 e2 e3) = do 
  _ <- check e1
  _ <- check e2
  _ <- check e3
  pure ()

check (Let v e1 e2) = do
  _ <- check e1 -- Evaluated in outer env
  _ <- localVars (varsExtend v) $ check e2 
  pure ()

check (ForLoop (v1, e1) (v2, e2) e3) = do
  simpleCheckHelper e1 e2 -- Evaluated in the outer env
  _ <- localVars (varsExtend v1 . varsExtend v2) $ check e3
  pure ()

check (Lambda v e) = do
  _ <- localVars (varsExtend v) $ check e
  pure ()

check (Apply e1 e2) = 
  simpleCheckHelper e1 e2

check (TryCatch e1 e2) = 
  simpleCheckHelper e1 e2

check (Print _ e) = do
  _ <- check e
  pure ()

check (KvPut e1 e2) = 
  simpleCheckHelper e1 e2

check (KvGet e) = do
  _ <- check e
  pure ()

checkExp :: Exp -> Maybe Error
checkExp e1 = 
  let CheckM f = check e1
  in case f [] of 
    Left err -> Just err
    Right () -> Nothing
