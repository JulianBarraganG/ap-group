module APL.AST
  (
  Exp(..),
  VName
  )
where

type VName = String

data Exp = 
  CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | ForLoop (VName, Exp) (VName, Exp) Exp
  -- ForLoop (p, initial) (i, bound) body
  deriving (Eq, Show)
