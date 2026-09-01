module APL.Eval
  (
  Val(..),
  eval,
  )
where

import APL.AST (Exp(..))

data Val = ValInt Integer
  deriving (Eq, Show)

eval :: Exp -> Val
eval expr =
  case expr of
  CnstInt x -> ValInt x

-- eval (CnstInt x) = ValInt x
-- ^^ why is this the same? ^^
