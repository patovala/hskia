--  File     : Stateguess.hs
--  Author   : Diana Barreto - Ivan Patricio Valero
--  Id       : 574386 -
--  Origin   : 06-May-2013
--  Purpose  : This module include the two main function
--             to execute the interval analysis
module TIntervalAnalysis (iterations, showintanalysis)

where
import Data.List
import TCFlow(CFGNode(..), PredCFGNode(..), pretyshow)
import TInterval(Interval(..),Lb(..),Ub(..))
import TEvalInterval (InterExp(..),transformExp, evalInterExp)
import Syntax (Exp(..),Opkind(..),Stmt(..))
import TVarStateOperations(entryState, getVarBottom, VarState(..),VarStates(..),
                          getVarTop, getUnionPredIntervals, convertVartoVal,
                          replaceVarVal,evalCondition, intersecVarState)

type Pos = Int

--Function iterations call the function iteration until
--the values are stabilized.
--The parameteras are:
--nodes with their predecessors,
--variables state before the next operation
--int i to take control of the number of operations
--[int] with the landmarks set 
iterations::[PredCFGNode]->VarStates->Int->[Int]->VarStates
iterations nodes x 0 lmarks	
   = let initialIteration = iteration nodes 0 lmarks []  (entryState (length nodes) (nub(getVarBottom nodes))) []
     in  iterations nodes initialIteration 1 lmarks 

iterations nodes stateIn i lmarks
   |i == 3 = 
      let newState = iteration nodes 0 lmarks [] stateIn []
          stateWidening = wideningState stateIn newState lmarks
      in
          iterations nodes stateWidening 1 lmarks 
   |otherwise =
     let newState = iteration nodes  0 lmarks [] stateIn []
     in 
        if(stateIn == newState) then
           newState
        else
           iterations nodes newState (i+1) lmarks
		   
--This Algorithm assign the value of the variable evaluating each node 
--The parameters are:
--nodes: the list of node that represent a tip program
--current: the id of the current node
--reachables: List of the nodes that are reachable in the 
--            current iteration
--intersect: It is the intersect nof the false brach evalueted
--           in a conditional node 
--stateOld : The state of the previous iterantion
--stateIn : where the state of the current iteration is build
iteration::[PredCFGNode]->Int->[Int]->VarState->VarStates->VarStates->VarStates
iteration [] _ _ _ _ stateIn = stateIn
iteration ((EntryNode,_):nodes) current _ _ stateOld stateIn
   =  iteration nodes  (current+1) ((current+1):[]) [] stateOld [nub(getVarTop nodes)]
iteration (((AsgNode var exp),n):nodes) current reachable intersect stateOld stateIn
   | elem current reachable
      = let 
           pastState = getUnionPredIntervals n stateOld stateIn 
           inter1 =  transformExp exp 
           inter2 =  convertVartoVal inter1 pastState
           inter3 =  evalInterExp inter2
           state = replaceVarVal pastState var inter3
        in iteration nodes  (current+1) ((current+1):reachable) [] stateOld (stateIn ++ [state])
    | otherwise 
      = let
           state2 = stateOld !! current
        in iteration nodes  (current+1) reachable [] stateOld (stateIn ++ [state2]) 
iteration (((IfGotoNode exp next),n):nodes)  current reachable intersect stateOld stateIn
    | elem current reachable
      = let 
           pastState = getUnionPredIntervals n stateOld stateIn
           eval = evalCondition exp pastState next (current+1) reachable
           trueIntersec = fst(fst eval)
           falseIntersec = snd(fst eval)
           newReachable = snd eval
           newState = intersecVarState pastState trueIntersec
        in iteration nodes (current+1) newReachable falseIntersec stateOld (stateIn ++ [newState])   
    |otherwise 
      = let 
           state = stateOld !! current
           eval = evalCondition exp state next (current+1) reachable
           falseIntersec = snd(fst eval)
        in iteration nodes (current+1) reachable falseIntersec stateOld (stateIn ++ [state])
iteration (((GotoNode next),n):nodes) current reachable intersect stateOld stateIn
    | elem current reachable
      = let
           pastState = getUnionPredIntervals n stateOld stateIn
           newState = intersecVarState pastState intersect 
        in iteration nodes (current+1) (next:reachable) [] stateOld (stateIn ++ [newState])
    | otherwise
      = let   
           state = stateOld !! current
        in
           iteration nodes (current+1) reachable [] stateOld (stateIn ++ [state])
iteration (((OutputNode exp),n):nodes)  current reachable intersect stateOld stateIn
    | elem current reachable
      = let
            state = getUnionPredIntervals n stateOld stateIn
        in  iteration nodes (current+1) ((current+1):reachable) [] stateOld (stateIn ++ [state])
    | otherwise
       = let         
            state = stateOld !! current
         in iteration nodes  (current+1) reachable [] stateOld (stateIn ++ [state])
iteration (((ExitNode),n):nodes)  current reachable intersect stateOld stateIn
    | elem current reachable
        = let
             pastState = getUnionPredIntervals n stateOld stateIn
          in (stateIn ++ [pastState])
    | otherwise
       = let         
            state = stateOld !! current
         in (stateIn ++ [state])

---------------Functions to do widening----------------------
wideningState::VarStates->VarStates->[Int]->VarStates
wideningState [] [] _ = []
wideningState (vs1:states1) (vs2:states2) x = 
    let vsfinal = wideningVars vs1 vs2 x
	in vsfinal:(wideningState states1 states2 x)


wideningVars::VarState->VarState->[Int]->VarState
wideningVars [] [] _ = []
wideningVars ((s1,i1):v1s) ((s2,i2):v2s) x=
   (s1,(wideningInterval i1 i2 x)):(wideningVars v1s v2s x)


wideningInterval::Interval->Interval->[Int]->Interval
wideningInterval Empty Empty _=
   Empty
   
wideningInterval (Interval lb1 ub1) (Interval lb2 ub2) []
   |lb1 <= lb2 =
      if (ub1>=ub2) then
         Interval lb1 ub1
      else
         Interval lb1 PlusInf
   |otherwise =
      if (ub1>=ub2) then
         Interval MinInf ub1
      else
         Interval MinInf PlusInf

wideningInterval (Interval lb1 ub1) (Interval lb2 ub2) list
   |lb1 <= lb2 =
      if (ub1>=ub2) then
         Interval lb1 ub1
      else
         Interval lb1 (minimumInterval (removeSmaller (valueUb ub2) list))
   |otherwise =
      if (ub1>=ub2) then
         Interval (maximumInterval(removeBigger (valueLb lb2) list)) ub1
      else
         Interval (maximumInterval(removeBigger (valueLb lb2) list)) (minimumInterval (removeSmaller (valueUb ub2) list))

maximumInterval::[Int]->Lb
maximumInterval [] = MinInf
maximumInterval list= Lb (maximum list) 

minimumInterval::[Int]->Ub
minimumInterval [] = PlusInf
minimumInterval list= Ub (minimum list)

removeSmaller::Int->[Int]->[Int]
removeSmaller _ []
   = []
removeSmaller y (x:xs) 
   = if(y>x) then
        removeSmaller y xs
     else
        x:(removeSmaller y xs)

removeBigger::Int->[Int]->[Int]
removeBigger _ [] = []
removeBigger y (x:xs) 
   = if(y<x) then
        removeBigger y xs
     else
        x:(removeBigger y xs)
-----------------------------------------------------------------
--tests
pruebaIterations::VarStates
pruebaIterations 
   =  let a = [0,1,10]
      in iterations pruebaList2 [] 0 a

pruebaList2::[PredCFGNode]   
pruebaList2 = [((EntryNode),[]),
               ((AsgNode "i" (Con 0)),[0]),
               ((AsgNode "j" (Con 0)),[1]),
               ((IfGotoNode (Op More (Con 10) (Var "i")) 5),[2,9]),
               ((GotoNode 10),[2,9]),
               ((AsgNode "i" (Op Plus (Var  "i") (Con 1))),[3]), 
               ((IfGotoNode Input 8),[5]),
               ((GotoNode 9),[5]),
               ((AsgNode "j" (Op Mult (Con 2) (Var "i"))),[6]),
               ((GotoNode 3),[8]),
               ((AsgNode "k" (Op Plus (Var "i") (Var "j"))),[4]),
               ((OutputNode (Var "k")),[10]),
               ((ExitNode),[11])]
-----------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PrettyPrints
-- Present the varstate output
--------------------------------------------------------------------------------
showintanalysis :: [(CFGNode, [Pos])] -> [VarState] -> Pos -> IO()
showintanalysis [] _ n = do putStr "" 
showintanalysis ((node, _):xs) (y:ys) n = do 
            putStrLn $ show n ++ ": \t\t\t| " ++ show n ++ " : "++ (showvars y) ++"\n"
            putStrLn $ "  " ++ (pretyshow node) ++ "\t|\t" ++ (pretyshow node)
            showintanalysis xs ys (n+1) 

showvars :: VarState -> String
showvars [] = ""
showvars ((v, i):xs) = do 
            show v ++ "=" ++ show i ++ showvars xs
