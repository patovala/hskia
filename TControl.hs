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
            putStrLn $ show n ++ ": " ++ show node ++ ":" ++ show ps 
            showctrp xs 
--extCtrPoint nodes n = foldl (\l (x:xs) n -> (n+1, x):l) [] nodes 

--map' f xs = foldr (\x acc -> f x : acc) [] xs
