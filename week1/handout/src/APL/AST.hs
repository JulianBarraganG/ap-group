module APL.AST
  (
  Exp(..)
  )
where

data Exp = 
  CnstInt Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  deriving (Eq, Show)
