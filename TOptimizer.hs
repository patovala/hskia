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
-- 1                    | 1: "i"=[-oo,oo] "j"=[-oo,oo] "k"=[-oo,oo]
--    i = "9"           |    i = "9"
-- 2                    | 2: "i"=[9,9] "j"=[-oo,oo] "k"=[-oo,oo]
--    if "i > 10" goto 4|    if "i > 10" goto 4
-- 3                    | 3: "i"=[9,9] "j"=[-oo,oo] "k"=[-oo,oo]
--    goto 7            |    goto 7
-- 4                    | 4: "i"=_|_ "j"=[-oo,oo] "k"=[-oo,oo]
--    j = "12"          |    j = "12"
-- 5                    | 5: "i"=_|_ "j"=_|_ "k"=_|_
--    k = "13"          |    k = "13"
-- 6                    | 6: "i"=_|_ "j"=_|_ "k"=_|_
--    goto 9            |    goto 9
-- 7                    | 7: "i"=_|_ "j"=_|_ "k"=_|_
--    j = "0"           |    j = "0"
-- 8                    | 8: "i"=_|_ "j"=[0,0] "k"=[-oo,oo]
--    k = "0"           |    k = "0"
-- 9                    | 9: "i"=_|_ "j"=[0,0] "k"=[0,0]
--    i = "12"          |    i = "12"
-- 10                    | 10: "i"=[12,12] "j"=[0,0] "k"=[0,0]
--    <exit>            |    <exit>
--
-- ... And, the resulting code should improvize the code to this:
-- 
-- 0: EntryNode -> []
-- 1: AsgNode "i" (Con 9) -> [0]
-- 7: AsgNode "j" (Con 0) -> [3]
-- 8: AsgNode "k" (Con 0) -> [7]
-- 9: AsgNode "i" (Con 12) -> [8,6]
-- 10: ExitNode -> [9]
--

module TOptimizer (removedead)
where
import TVarStateOperations(entryState, getVarBottom, VarState(..),VarStates(..),
                          getVarTop, getUnionPredIntervals, convertVartoVal,
                          replaceVarVal,evalCondition, intersecVarState)
import TControl(SCFGNode(..))
import TCFlow(CFGNode(..))
import TInterval(Interval(..))


-- TODO: this imports must be analyzed before integration

-- Function that receives the SCFGNodes and varstates and returns the SCFGNodes 
-- without the dead code in terms of non reacheability
removedead :: [SCFGNode] -> VarStates -> [SCFGNode]
--removedead nodes = renumerate . filtergotos . filterdead . filterbottom nodes 
removedead nodes = filterbottom nodes 

-- filter the bottom vars 
filterbottom :: [SCFGNode] -> VarStates -> [SCFGNode]
filterbottom _ [] = []
filterbottom (x:xs)(y:ys)  
        | isbottom y = filterbottom xs ys
        | otherwise = x : (filterbottom xs ys)

-- report all dead nodes from unreachable branches 
isbottom :: VarState -> Bool
isbottom [] = True
isbottom ((x, Empty):xs) = True && isbottom xs
isbottom ((x, _):xs) = False && isbottom xs

-- filter all dead gotos (ifs)  pointing to a non existent node
-- filter also gotos pointing to consecutives nodes
filterdead :: [SCFGNode] -> [SCFGNode]
filterdead testnodes = filter istherefunc testnodes 
    where istherefunc (_, IfGotoNode _ w) = elem w nodelist 
          istherefunc _ = True 
          nodelist = [n | (n, _) <- testnodes]

-- filter consecutive gotos nodes
filtergotos :: [SCFGNode] -> [SCFGNode]
filtergotos [] = []
filtergotos (x@(_, GotoNode n):(m, _):xs) 
    | n == m = (filtergotos xs)
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
          findnode m = if ((matches m) /= []) then snd ( head (matches m)) else -1
          matches m = filter (\(k, v) -> m == k) $ nodepairs
          -- findnode m = m 


-- Se pierden las referencias así que hay que manejar caso por caso y reestructurar
-- todo, justo despues de que ha eliminado algún nodo, se debe correr un verificador
-- que nos diga cuando un nodo ha desaparecido y ya no es alcanzable, se debe 
-- eliminar toda esa rama, y volver nuevamente a rodar el comprobador de sentido
-- el comprobador de sentido recursivo me dice hasta cuando se debe volver a 
-- intentar conseguir semántica en el programa
