module TControl 
where

import TCFlow(evalwrpr, eval, CFGNode(..), pprint)
import TParse (tParse)

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

{-- Test Data --}
myprogram = "n=0;while(n>100){n=n-1;}"
productions' = eval (tParse myprogram) 1  -- 1 cause entry 
productions = evalwrpr productions'        -- Node has not 
-- PV use this as a test for feeding later
sproductions = putids productions 0
myresp = Response [] [] [] [] []
{-- Fin Test Data --}

-- extCtrPoint :: [CFGNode] -> [CtrPoint]
-- extCtrPoint [] = []
-- extCtrPoint x (_:y:[]) = [] -- get the before last node
-- extCtrPoint x (y:x:xs) = 

getPred :: CFGNode -> Pos -> [CFGNode] -> Predecesors
getPred node idn [] = []
getPred node idn (y@(AsgNode _ _):x:xs) 
        | node == x = y:getPred node idn xs
getPred node idn (y@(GotoNode to):xs) 
        | idn == to = y:getPred node idn xs
        | otherwise = getPred node idn xs
getPred node idn (x:xs) = getPred node idn xs


extCtrPoint :: [CFGNode] -> Int -> [CtrPoint] 
extCtrPoint [] n = []
extCtrPoint ((GotoNode _):xs) n = extCtrPoint xs (n+1)
extCtrPoint (x:xs) n = (CtrPoint n x points):(extCtrPoint xs (n+1))
        where points = getPred x n productions 

showctrp :: [CtrPoint] -> IO()
showctrp [] = do putStrLn "" 
showctrp ((CtrPoint n node ps):xs) = do 
            putStrLn $ show n ++ ": " ++ show node ++ " -> " ++ show ps 
            showctrp xs 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- New approach to get predesesors and sucesors
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
type SCFGNode = (Int, CFGNode) -- an improved CFGNode with id
type N_Succ = [SCFGNode]
type N_Pred = [SCFGNode]
data Node 
        = Node SCFGNode N_Succ N_Pred
        deriving (Show)
type Entries = [Node]
type Assignments = [Node]
type Tests = [Node]
type Junctions = [Node]
type Exits = [Node]
data Response 
        = Response Entries Assignments Tests Junctions Exits
        deriving (Show)

-- putids: helping program to put id to the list of CFGNode
putids :: [CFGNode] -> Pos -> [SCFGNode]
putids [] n = []
putids (x:xs) n = (n, x):putids xs (n+1)

---8<--------------------------------------------------------------------------
-- PV process to find sucesors and predecesors 
--
-- entry nodes
sucprede :: [SCFGNode] -> Response -> Response 
sucprede [] r = r
sucprede (x@(_, EntryNode):y:xs) r = sucprede (y:xs) r'
                where
                    r' = (Response entries' a t j e) 
                    (Response entries a t j e) = r
                    entries' = (Node x [y] []) : entries
sucprede (x:xs) r = sucprede xs r

-- assignment nodes
sucpreda :: [SCFGNode] -> Response -> Response 
sucpreda [] r = r
sucpreda (x:y@(n, AsgNode _ _):z:xs) r = sucpreda (y:z:xs) r'
                where
                    r' = (Response es a' t j e)
                    (Response es a t j e) = r
                    a' = (Node y [z] (tpred x)) : a
                    tpred (n2, GotoNode n1) = if n1 == n then [x] else []
                    tpred x' = [x]
sucpreda (x:xs) r = sucpreda xs r

-- test nodes, a test has a predecesor and two sucessors
sucpredt :: [SCFGNode] -> Response -> Response 
sucpredt [] r = r
sucpredt (x:y@(n, IfGotoNode _ _):z1:z2:xs) r = sucpredt (y:z1:z2:xs) r'
                where
                    r' = (Response es a t' j e)
                    (Response es a tests j e) = r
                    t' = (Node y [x] (ifsucc y)) : tests
                    ifsucc y = [n_succ_t, n_succ_f]
                    n_succ_t = z2 
                    n_succ_f = z1
sucpredt (x:xs) r = sucpredt xs r

-- get the exit nodes
sucpredex :: [SCFGNode] -> Response -> Response 
sucpredex [] r = r
sucpredex (x:y@(n, ExitNode):xs) r = sucpredex (y:xs) r'
                where
                    r' = (Response e a t j ex')
                    (Response e a t j ex) = r
                    ex' = (Node y [] (tpred x)) : ex
                    tpred (n2, GotoNode n1) = if n1 == n then [x] else []
                    tpred _ = [x]
sucpredex (x:xs) r = sucpredex xs r

-- Get all the previous node from a node that is not an entry and also
-- has not previous gotos
getjunct :: [SCFGNode] -> [SCFGNode] -> Junctions  
getjunct [] _ = []
getjunct (x@(_, GotoNode _):y:xs) ns = getjunct (y:xs) ns
--getjunct (x@(_, IfGotoNode _ _):y:xs) ns = getjunct (y:xs) ns
--getjunct (x@(_, EntryNode):y:xs) ns = getjunct (y:xs) ns
getjunct (x:y@(_, GotoNode _):xs) ns = getjunct (xs) ns
--getjunct (x:y@(_, IfGotoNode _ _):xs) ns = getjunct (xs) ns
getjunct (x:y@(_, EntryNode):xs) ns = getjunct (xs) ns
getjunct (x:y:z:xs) ns = nextnode : getjunct (y:z:xs) ns
                where
                    nextnode = (Node y [z] predecesors)
                    predecesors = [x] ++ getmorepredecesors y ns
getjunct (x:xs) ns = getjunct xs ns -- last resource

-- Get predecesors gotos that points to this node
getmorepredecesors :: SCFGNode -> [SCFGNode] -> N_Pred 
getmorepredecesors _ [] = [] 
getmorepredecesors x@(n, _) (y@(_, GotoNode n1):ys)
        | n == n1 = y : getmorepredecesors x ys
        | otherwise = getmorepredecesors x ys
getmorepredecesors x@(n, _) (y@(_, IfGotoNode _ n1):ys)
        | n == n1 = y : getmorepredecesors x ys
        | otherwise = getmorepredecesors x ys
getmorepredecesors x (y:ys) = getmorepredecesors x ys

-- merge all in the response
mergeresp sprod = rx
        where 
            rj = (Response [] [] [] juncs [])
            juncs = getjunct sprod sprod
            re = sucprede sprod rj
            ra = sucpreda sprod re
            rt = sucpredt sprod ra
            rx = sucpredex sprod rt
