module TControl (getconst, getconst', getctrpoints, showctrpoints, 
                putids, Pos, SCFGNode, spprint2, spprint)
where

import TCFlow(evalwrpr, eval, CFGNode(..), pprint, pretyshow)
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

type SCFGNode = (Int, CFGNode) -- an improved CFGNode with id
{-- Test Data --}
--myprogram = "n=0;while((100-97)>n){n=n-1*99;}"
--productions' = eval (tParse myprogram) 1  -- 1 cause entry 
--productions = evalwrpr productions'        -- Node has not 
---- PV use this as a test for feeding later
--sproductions = putids productions 0
{-- Fin Test Data --}
 
-------------------------------------------------------------------------------
-- New approach gettin only a tuple with ids
-- PV
-------------------------------------------------------------------------------

-- putids: helping program to put id to the list of CFGNode
putids :: [CFGNode] -> Pos -> [SCFGNode]
putids [] n = []
putids (x:xs) n = (n, x):putids xs (n+1)

-- This is the function to getctrlpoints
getctrpoints :: [SCFGNode] -> [SCFGNode] -> [(CFGNode, [Pos])]
getctrpoints [] sproductions = []
getctrpoints (x:xs) sproductions = cnode : getctrpoints xs sproductions
        where (pos, cfgnode) = x
              cnode = (cfgnode, (points x))
              points x = getpred2 x sproductions sproductions

-- This function shows the new control points with predecesors ids
showctrpoints :: [(CFGNode, [Pos])] -> Pos -> IO()
showctrpoints [] n = do putStrLn "" 
showctrpoints ((node, points):xs) n = do 
            putStrLn $ show n ++ ": " ++ show node ++ " -> " ++ show points 
            showctrpoints xs (n+1) 

--
-- PV All previous node is a predecesor except a goto and if node who has to
-- be evaluated in a different way
getpred2 :: SCFGNode -> [SCFGNode] -> [SCFGNode] -> [Pos]
getpred2 node [] _ = []
-- getpred2 node ((n, IfGotoNode _ n1):y:xs) 
--         | node == y = nodelist ++ getpred2 node (y:xs)
--         where nodelist = getgotopred node sproductions  
getpred2 node ((n, GotoNode n1):y:xs) sproductions 
        | node == y = nodelist ++ getpred2 node (y:xs) sproductions
        where nodelist = getgotopred node sproductions  
getpred2 node (x@(n, _):y:xs) sproductions
        | node == y = n:maybegotos ++ getpred2 node (y:xs) sproductions
        where maybegotos = getgotopred y sproductions
getpred2 node (x:xs) sproductions = getpred2 node (xs) sproductions

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
getconst :: [SCFGNode] -> [Int]
getconst [] = []
getconst (x@(_, AsgNode _ exp):xs) = cons ++ (getconst xs) 
        where cons = filtercons exp 
              (_, cfgnode) = x
getconst (x@(_, IfGotoNode exp _):xs) = cons ++ (getconst xs) 
        where cons = filtercons exp 
              (_, cfgnode) = x
getconst (x:xs) = getconst xs

filtercons :: Exp -> [Int] 
filtercons (Con i) = [i]
filtercons (Op _ e1 e2) = filtercons e1 ++ filtercons e2
filtercons x = [] 

-- This is a wrapper for avoid using a more elegant and elaborated SCFGNode
getconst' cfgnodes = getconst (putids cfgnodes 0)

--
-- Print the SCFNode 
--
spprint :: [SCFGNode] -> IO()
spprint [] = do putStrLn "" 
spprint ((n, x):xs) = do 
            putStrLn $ show n ++ ":" ++ pretyshow x 
            spprint xs 

--let maxcolsize = maximum [length(pretyshow x) | (_, x) <- sproductions]
--
-- Print the SCFNode for Optimization option
--
spprint2 prevs nexts = sopprint prevs nexts max
        where max = maximum [length(pretyshow x) | (_, x) <- prevs] 

sopprint :: [SCFGNode] -> [SCFGNode] -> Int -> IO()
sopprint [] [] max         = do putStr "" 
sopprint [] (y:ys) max     = do putStrLn $ (filler max) ++ "|" ++ (printelem y max)
                                sopprint [] ys max 
sopprint (x:xs) [] max     = do putStrLn $ (printelem x max) ++ "|" ++ filler max
                                sopprint xs [] max 
sopprint (x:xs) (y:ys) max = do 
                putStrLn $ (printelem x max) ++ "|" ++ (printelem y max) 
                sopprint xs ys max 

printelem :: SCFGNode -> Int -> String
printelem (n, x) max = pnode ++ filler len
        where
            pnode = show n ++ ":" ++ pretyshow x
            len = max - length (pnode)

filler n = replicate (n+8) ' '

