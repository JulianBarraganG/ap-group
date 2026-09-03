module APL.AST
  (
  Exp(..),
  VName
  )
where

type VName = String

data Exp = 
  CnstInt Integer
  | CnstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  deriving (Eq, Show)
