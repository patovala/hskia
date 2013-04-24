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
-- n is a node counter...
----------------------------------------------------------------
eval :: [Stmt] -> Int -> [CFGNode]
eval [] n = [] 
eval ((Asg v e):xs) n = (AsgNode v e) : eval xs (n + 1)
eval ((If e es):xs) n = (IfGotoNode e (n + 1)) : eval xs (n + 2)
eval ((Ite e es1 es2):xs) n = (IfGotoNode e (n + 1)): (GotoNode (n + 2)) : eval xs (n + 3)

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
    where val = eval (tParse s) 0

doto s = eval (tParse s) 0
