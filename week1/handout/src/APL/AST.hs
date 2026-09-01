module APL.AST
  (
  Exp(..)
  )
where

data Exp = 
  CnstInt Integer
  | CnstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  deriving (Eq, Show)
