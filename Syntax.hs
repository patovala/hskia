module Syntax
where

import Data.List

{-------------------------------------------------------------------

  Data types for ASTs for a TIP fragment, for use with the 2013
  project in COMP90053.

-------------------------------------------------------------------}

data Exp
  = Con Int
  | Var String
  | Op Opkind Exp Exp
  | Input
  deriving Eq

instance Show Exp where
    show (Con i) = show i
    show (Var s) = s
    show (Op opkind e1 e2) = show e1 ++ " " ++ show opkind ++ " " ++ show e2
    show Input = "input"

data Opkind
  = Plus | Minus | Mult | Div | More | Equal | Nequal | Lequal
  deriving Eq

instance Show Opkind where
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show More = ">"
    show Equal = "="
    show Nequal = "!="
    show Lequal = "<="

data Stmt
  = Asg String Exp
  | If Exp [Stmt]
  | Ite Exp [Stmt] [Stmt]
  | While Exp [Stmt]
  | Output Exp
  deriving (Show,Eq)

notIn :: String -> Exp -> Bool
_ `notIn` (Con _) 
  = True
x `notIn` (Var s) 
  = x /= s
x `notIn` (Op _ e1 e2)
  = x `notIn` e1 && x `notIn` e2
_ `notIn` Input
  = True

vars :: Exp -> [String]
vars (Con _)
  = []
vars (Var s)
  = [s]
vars (Op _ e1 e2)
  = sort $ nub $ (vars e1 ++ vars e2)
vars Input
  = []

varExps :: Exp -> [Exp]
varExps e
  = map Var (vars e)
