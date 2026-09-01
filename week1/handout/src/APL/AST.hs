module APL.AST
  (
  Exp(..)
  )
where

data Exp = CnstInt Integer
  deriving (Eq, Show)
