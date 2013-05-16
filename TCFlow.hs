module TCFlow (doit, doto, flowFile, CFGNode(..), evalwrpr, eval, pprint, pretyshow, PredCFGNode)
where

import Syntax
import TParse (tParse, parseFile, prex)

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

--Data Type that agrupe a node and its predecessors
type PredCFGNode = (CFGNode,[Int])

--define a SCFGNode
type SCFGNode = (Int, CFGNode)


-------------------------------------------------------------------------------
-- PV Given an Expresion return the list of nodes
--
-- n is a node counter...
-------------------------------------------------------------------------------
eval :: [Stmt] -> Int -> [CFGNode]
eval [] n = [] 
eval ((Asg var exp):xs) n = (AsgNode var exp) : eval xs (n + 1)
eval ((Output exp):xs) n = (OutputNode exp) : eval xs (n + 1)
eval ((If exp stmts):xs) n = 
          (IfGotoNode exp (n + 2)):(GotoNode next): inner_stmts ++ eval xs next
		where next = length inner_stmts + (n + 2)  -- 2 lines ahead
		      inner_stmts = eval stmts (n + 2) 
eval ((Ite exp stmts1 stmts2):xs) n = 
          (IfGotoNode exp nif):(GotoNode nelse):inner_stmts1 
                      ++ (GotoNode end):inner_stmts2 
                      ++ eval xs end
		where nelse = length inner_stmts1 + 1 + nif 
		      inner_stmts1 = eval stmts1 nif 
		      end = length inner_stmts2  + nelse 
		      inner_stmts2 = eval stmts2 nelse 
                      nif = n + 2
eval ((While exp stmts):xs) n = 
                (IfGotoNode exp n2) : (GotoNode next): inner_stmts ++ 
                [(GotoNode n)] ++ eval xs next
                where next = n2 + 1 + length inner_stmts 
		      inner_stmts = eval stmts n2
                      n2 = n + 2

evalwrpr productions = EntryNode:productions++[ExitNode]  
----------------------------------------------------------------
--
--  Test: try to get the CFGNode list from the parser
--
--------------------------------------------------------------------------------
doit s = show val 
    where val = eval (tParse s) 0

doto s = eval (tParse s) 0

--
-- Node sequence productions pretty print 
--
pprint :: [CFGNode] -> Int -> IO()
pprint [] n = do putStrLn "" 
pprint (x:xs) n = do 
            putStrLn $ show n ++ ":" ++ pretyshow x 
            pprint xs (n + 1)

flowFile :: FilePath -> IO() 
flowFile fp
 = do tipProgram <- readFile fp
      let productions = eval (tParse tipProgram) 1  -- 1 cause entry 
      let productions' = evalwrpr productions       -- Node has not 
      pprint productions' 0                         -- yet there

pretyshow :: CFGNode -> String
pretyshow (AsgNode str expr) = str ++ " = " ++ show (prex expr)
pretyshow (OutputNode expr) = "output " ++ show (prex expr)
pretyshow (GotoNode n) = "goto " ++ show n
pretyshow (IfGotoNode expr n) = "if "++ show (prex expr) ++ " goto " ++ show n
pretyshow (EntryNode) = "<entry>"
pretyshow (ExitNode) = "<exit>"

