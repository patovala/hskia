--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : Implementation of Optimizer using the Interval Analysis 
--------------------------------------------------------------------------------
-- We are thinking about doing a code optimization that helps eliminating 
-- unreachable code, our first approach could be to use the interval analysis
-- output to get clues about what could be improvized.
-- 
-- For example, given a program like this:

-- i=9;
-- if (i>10){
--     j=12;
--     k=13;
-- }else{
--     j=0;
--     k=0;
-- }
-- i=12;
-- 
-- our production algorithm generates this linearization:
--
-- 0:<entry>
-- 1:i = "9"
-- 2:if "i > 10" goto 4
-- 3:goto 7
-- 4:j = "12"
-- 5:k = "13"
-- 6:goto 9
-- 7:j = "0"
-- 8:k = "0"
-- 9:i = "12"
-- 10:<exit>
--
-- The widening and interval analysis generates this:
-- 
-- 0                    | 0:
--    <entry>           |    <entry>
-- 1                    | 1: i=[-oo,oo] j=[-oo,oo] k=[-oo,oo]
--    i = 9           |    i = 9
-- 2                    | 2: i=[9,9] j=[-oo,oo] k=[-oo,oo]
--    if i > 10 goto 4|    if i > 10 goto 4
-- 3                    | 3: i=[9,9] j=[-oo,oo] k=[-oo,oo]
--    goto 7            |    goto 7
-- 4                    | 4: i=_|_ j=[-oo,oo] k=[-oo,oo]
--    j = 12          |    j = 12
-- 5                    | 5: i=_|_ j=_|_ k=_|_
--    k = 13          |    k = 13
-- 6                    | 6: i=_|_ j=_|_ k=_|_
--    goto 9            |    goto 9
-- 7                    | 7: i=_|_ j=_|_ k=_|_
--    j = 0           |    j = 0
-- 8                    | 8: i=_|_ j=[0,0] k=[-oo,oo]
--    k = 0           |    k = 0
-- 9                    | 9: i=_|_ j=[0,0] k=[0,0]
--    i = 12          |    i = 12
-- 10                    | 10: i=[12,12] j=[0,0] k=[0,0]
--    <exit>            |    <exit>
--
-- ... And, the resulting code should be improvised like this: 
--
-- 0:<entry>               |0:<entry>               
-- 1:i = 9                 |1:i = 9                 
-- 2:if i > 10 goto 4      |2:k = 0                 
-- 3:goto 7                |3:i = 12                
-- 4:j = 12                |4:<exit>                
-- 5:k = 13                |                        
-- 6:goto 9                |                        
-- 7:j = 0                 |                        
-- 8:k = 0                 |                        
-- 9:i = 12                |                        
-- 10:<exit>               |                        
--

module TOptimizer (removedead)
where
import TVarStateOperations(entryState, getVarBottom, VarState(..),VarStates(..),
                          getVarTop, getUnionPredIntervals, convertVartoVal,
                          replaceVarVal,evalCondition, intersecVarState)
import TControl(SCFGNode(..))
import TCFlow(CFGNode(..))
import TInterval(Interval(..),AbsValue(..))
import Data.List(intersect)

-- Function that receives the SCFGNodes and varstates and returns the SCFGNodes 
-- without the dead code in terms of non reacheability
removedead :: [SCFGNode] -> VarStates -> [SCFGNode]
--removedead nodes = renumerate . filtergotos . filterdead . filterbottom nodes 
removedead nodes = renumerate . filtergotos . filterdead . filterbottom nodes 

-- filter the unreachable vars 
filterbottom :: [SCFGNode] -> VarStates -> [SCFGNode]
filterbottom _ [] = []
filterbottom (x:xs)(y:ys)  
        | isNoReach y = filterbottom xs ys
        | otherwise = x : (filterbottom xs ys)

-- report all dead nodes from unreachable branches 
isNoReach :: VarState -> Bool
isNoReach [] = True
isNoReach ((x, NoReach):xs) = True && isNoReach xs
isNoReach ((x, _):xs) = False && isNoReach xs

-- filter all dead gotos (ifs)  pointing to a non existent node
-- filter also gotos pointing to consecutives nodes
filterdead :: [SCFGNode] -> [SCFGNode]
filterdead testnodes = if (testnodes == testnodes2) then testnodes
                       else filterdead testnodes2 
    where testnodes2 = filter istherefunc testnodes 
          istherefunc (_, IfGotoNode _ w) = elem w nodelist 
          istherefunc (_, GotoNode w) = elem w nodelist 
          istherefunc _ = True 
          nodelist = [n | (n, _) <- testnodes]

-- filter consecutive gotos nodes (experimental)
filtergotos :: [SCFGNode] -> [SCFGNode]
filtergotos [] = []
filtergotos (x@(_, GotoNode n):(m, _):xs) 
    | n == m  = (filtergotos xs)
filtergotos (x:xs) = x : (filtergotos xs)

-- PV
-- renumerate the nodes to get a better shape of numbers
renumerate :: [SCFGNode] -> [SCFGNode]
renumerate nodes = zip [0..] (map fixgotofunc cfgnodes)
    where 
          cfgnodes = [node | (_, node) <- nodes] 
          nodepairs = zip [i | (i, _) <- nodes] [0..] 
          fixgotofunc (IfGotoNode e n) = (IfGotoNode e (findnode n)) 
          fixgotofunc (GotoNode n) = (GotoNode (findnode n)) 
          fixgotofunc s = s 
          -- TODO: test this
          findnode m = if ((matches m) /= []) then snd ( head (matches m)) else -1
          matches m = filter (\(k, v) -> m == k) $ nodepairs
          -- findnode m = m 

-- assert that all the gotos have nodes or keep filtering 
assert :: [SCFGNode] -> [SCFGNode]
assert nodes 
    | (intersect nodelist gotonodelist ) == gotonodelist = nodes
    | otherwise = assert $ filterdead nodes
    where
        nodelist = map fst nodes
        gotonodelist = onlygotofunc nodes 
        onlygotofunc ((_, GotoNode i):xs) = i : onlygotofunc xs 
        onlygotofunc ((_, IfGotoNode _ i):xs) = i : onlygotofunc xs
        onlygotofunc (_:xs) = onlygotofunc xs
        onlygotofunc [] = [] 

