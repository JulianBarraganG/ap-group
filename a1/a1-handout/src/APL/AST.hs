module APL.AST
  (
  Exp(..),
  VName,
  printExp,
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
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

wrapPrnths :: String -> String
wrapPrnths input = "(" ++ input ++ ")"

printExp :: Exp -> String
printExp (CstBool True) = "true"
printExp (CstBool False) = "false"
printExp (CstInt n) = show n
-- ARITHMETICS BABAAAE
printExp (Add e1 e2) = wrapPrnths(printExp e1 ++ " + " ++ printExp e2)
printExp (Sub e1 e2) = wrapPrnths(printExp e1 ++ " - " ++ printExp e2)
printExp (Mul e1 e2) = wrapPrnths(printExp e1 ++ " * " ++ printExp e2)
printExp (Div e1 e2) = wrapPrnths(printExp e1 ++ " / " ++ printExp e2)
printExp (Pow e1 e2) = wrapPrnths(printExp e1 ++ " ** " ++ printExp e2)
printExp (Eql e1 e2) = wrapPrnths(printExp e1 ++ " == " ++ printExp e2)
printExp (If e1 e2 e3) = wrapPrnths(
    "if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3
  )
printExp (Let vname e1 e2) = wrapPrnths(
    "let " ++ vname ++ " = " ++ printExp e1 ++ " in " ++ printExp e2
  )
printExp (Var vname) = vname
printExp (ForLoop (p, initial) (i, bound) body) = wrapPrnths(
    "loop " ++ p ++ " = " ++ printExp initial ++ " for " ++ i ++ " < " ++ printExp bound ++ " do " ++ printExp body
  )
printExp (Lambda vname e1) = wrapPrnths(
    "\\" ++ vname ++ " -> " ++ printExp e1
  )
printExp (Apply e1 e2) = wrapPrnths(printExp e1 ++ " " ++ printExp e2)
printExp (TryCatch e1 e2) = wrapPrnths("try " ++ printExp e1 ++ " catch " ++ printExp e2)
