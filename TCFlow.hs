module TCFlow (doit)
where

import Syntax
import TParse (tParse)

{-------------------------------------------------------------------

  The control flow implementation for the COMP90053 project.

  Authors:
    Diana Barreto
    Ivan Valarezo (601099)

-------------------------------------------------------------------}

--
-- PV The node to hold our productions
--
data CFGNode
    = AsgNode String Exp 
    | OutputNode Exp
    | GotoNode Int
    | IfGotoNode Exp Int 
    | EntryNode
    | ExitNode
    deriving (Show,Eq)


----------------------------------------------------------------
-- PV Given an Expresion return the list of nodes
--
--
----------------------------------------------------------------
eval :: [Stmt] -> [CFGNode]
eval [] = [] 
eval ((Asg v e):xs) = (AsgNode v e) : eval xs

-- = Asg String Exp
-- | If Exp [Stmt]
-- | Ite Exp [Stmt] [Stmt]
-- | While Exp [Stmt]
-- | Output Exp


----------------------------------------------------------------
--
--  Test: try to get the CFGNode list from the parser
--
----------------------------------------------------------------
doit s = show val 
    where val = eval (tParse s)
