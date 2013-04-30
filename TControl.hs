module TControl 
where

import TCFlow(evalwrpr, eval, CFGNode(..), pprint)
import TParse (tParse)
import Syntax (Exp(..))

{-----------------------------------------------------------------------------

    Trying to get the control points 
  Authors:
    Diana Barreto
    Ivan Valarezo (601099)

-----------------------------------------------------------------------------}

-- So we need the control points or the antesesors?
-- 1. get a list of antecesors for each elements of our production
--    this could be a structure of nodes inside a list
type Predecesors = [CFGNode]
type Pos = Int 
data CtrPoint = 
        CtrPoint Int CFGNode Predecesors 
        deriving(Show)

type SCFGNode = (Int, CFGNode) -- an improved CFGNode with id
{-- Test Data --}
myprogram = "n=0;while((100-97)>n){n=n-1*99;}"
productions' = eval (tParse myprogram) 1  -- 1 cause entry 
productions = evalwrpr productions'        -- Node has not 
-- PV use this as a test for feeding later
sproductions = putids productions 0
{-- Fin Test Data --}

-- extCtrPoint :: [CFGNode] -> [CtrPoint]
-- extCtrPoint [] = []
-- extCtrPoint x (_:y:[]) = [] -- get the before last node
-- extCtrPoint x (y:x:xs) = 

--getpred :: CFGNode -> Pos -> [CFGNode] -> Predecesors
--getpred node idn [] = []
--getpred node idn (y@(AsgNode _ _):x:xs) 
--        | node == x = y:getpred node idn xs
--getpred node idn (y@(GotoNode to):xs) 
--        | idn == to = y:getpred node idn xs
--        | otherwise = getpred node idn xs
--getpred node idn (x:xs) = getpred node idn xs
--
--extCtrPoint :: [CFGNode] -> Int -> [CtrPoint] 
--extCtrPoint [] n = []
--extCtrPoint ((GotoNode _):xs) n = extCtrPoint xs (n+1)
--extCtrPoint (x:xs) n = (CtrPoint n x points):(extCtrPoint xs (n+1))
--        where points = getpred x n productions 
--
--showctrp :: [CtrPoint] -> IO()
--showctrp [] = do putStrLn "" 
--showctrp ((CtrPoint n node ps):xs) = do 
--            putStrLn $ show n ++ ": " ++ show node ++ " -> " ++ show ps 
--            showctrp xs 
-------------------------------------------------------------------------------
-- New approach gettin only a tuple with ids
-- PV
-------------------------------------------------------------------------------

-- putids: helping program to put id to the list of CFGNode
putids :: [CFGNode] -> Pos -> [SCFGNode]
putids [] n = []
putids (x:xs) n = (n, x):putids xs (n+1)

-- This is the main function
getctrpoints :: [SCFGNode] -> [(CFGNode, [Pos])]
getctrpoints [] = []
getctrpoints (x:xs) = cnode : getctrpoints xs
        where (pos, cfgnode) = x
              cnode = (cfgnode, (points x))
              points x = getpred2 x sproductions

-- This function shows the new control points with predecesors ids
showctrpoints :: [(CFGNode, [Pos])] -> Pos -> IO()
showctrpoints [] n = do putStrLn "" 
showctrpoints ((node, points):xs) n = do 
            putStrLn $ show n ++ ": " ++ show node ++ " -> " ++ show points 
            showctrpoints xs (n+1) 

--
-- PV All previous node is a predecesor except a goto and if node who has to
-- be evaluated in a different way
getpred2 :: SCFGNode -> [SCFGNode] -> [Pos]
getpred2 node [] = []
-- getpred2 node ((n, IfGotoNode _ n1):y:xs) 
--         | node == y = nodelist ++ getpred2 node (y:xs)
--         where nodelist = getgotopred node sproductions  
getpred2 node ((n, GotoNode n1):y:xs)  
        | node == y = nodelist ++ getpred2 node (y:xs)
        where nodelist = getgotopred node sproductions  
getpred2 node (x@(n, _):y:xs) 
        | node == y = n:maybegotos ++ getpred2 node (y:xs)
        where maybegotos = getgotopred y sproductions
getpred2 node (x:xs) = getpred2 node (xs)

-- Get predecesors gotos that points to this node
getgotopred :: SCFGNode -> [SCFGNode] -> [Pos] 
getgotopred _ [] = [] 
getgotopred x@(n, _) ((n2, GotoNode n1):ys)
        | n == n1 = n2 : getgotopred x ys
--        | otherwise = getgotopred x ys
getgotopred x@(n, _) ((n2, IfGotoNode _ n1):ys)
        | n == n1 = n2 : getgotopred x ys
--        | otherwise = getgotopred x ys
getgotopred x (y:ys) = getgotopred x ys

-------------------------------------------------------------------------------
-- End new approach
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- PV Get the constants from a production
-- A constant should be get from the expresions
-------------------------------------------------------------------------------
getconst :: [SCFGNode] -> [(CFGNode, [Pos])]
getconst [] = []
getconst (x@(_, AsgNode _ exp):xs) = (cfgnode, cons) : getconst xs 
        where cons = filtercons exp 
              (_, cfgnode) = x
getconst (x@(_, IfGotoNode exp _):xs) = (cfgnode, cons) : getconst xs 
        where cons = filtercons exp 
              (_, cfgnode) = x
getconst (x:xs) = getconst xs

filtercons :: Exp -> [Int] 
filtercons (Con i) = [i]
filtercons (Op _ e1 e2) = filtercons e1 ++ filtercons e2
filtercons x = [] 

-- This is a wrapper for avoid using a more elegant and elaborated SCFGNode
getconst' cfgnodes = getconst (putids cfgnodes 0)
