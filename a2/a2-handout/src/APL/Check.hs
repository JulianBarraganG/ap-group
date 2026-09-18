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

check :: Exp -> CheckM ()
check (CstInt _) = pure ()

check (CstBool _) = pure ()

check (Var v) = do
  vars <- askVars
  if (elem v vars) then pure() else (noVars v)

check (Add e1 e2) = do 
  _ <- check e1
  _ <- check e2
  pure ()

check _ = undefined

checkExp :: Exp -> Maybe Error
checkExp e1 = 
  let CheckM f = check e1
  in case f [] of 
    Left err -> Just err
    Right () -> Nothing
