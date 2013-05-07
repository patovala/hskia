----------------------------------------------------------------
--
--  Hskia: A simple interval analyser for tip programs
--  
--  To compile:  ghc --make Main.hs -o hskia
--
--  (c) 
--  Diana Barreto - Ivan Valarezo
--
----------------------------------------------------------------

module Main (main) 
where

import TCFlow(flowFile, evalwrpr, eval, CFGNode(..), PredCFGNode(..), pretyshow)
import TParse (tParse, pretty, parseFile)
import TControl (showctrpoints, getctrpoints, putids, Pos, getconst)
import TInterval(Interval(..),Lb(..),Ub(..))
import System.Environment (getProgName, getArgs)
import TEvalInterval (InterExp(..),transformExp, evalInterExp)
import Data.List
import Syntax (Exp(..),Opkind(..),Stmt(..))
import TVarStateOperations(entryState, getVarBottom, VarState(..),VarStates(..),
                          getVarTop, getUnionPredIntervals, convertVartoVal,
                          replaceVarVal,evalCondition, intersecVarState)

help :: String -> IO ()
help tipprog 
  = do
    putStrLn ("Usage: " ++ tipprog ++ " [-t -p -a] filename\n")
    putStrLn ("      -a: get the production list from file\n ")
    putStrLn ("      -p: get the predecesors list from file\n ")
    putStrLn ("      -i: start the interval\n ")

main :: IO ()
main 
  = do
      tipprog <- getProgName
      args <- getArgs
      processArgs tipprog args 

processArgs :: String -> [String] -> IO ()
processArgs _ [inputFile]
  = do
      flowFile inputFile
processArgs tipprog [option, inputFile] 
  = do
      case option of
            "-a" -> flowFile inputFile 
            "-p" -> doFilePred inputFile 
            "-i" -> doInterval inputFile 
            _ -> help tipprog

processArgs tipprog _
  = do
      help tipprog

doFilePred :: FilePath -> IO() 
doFilePred fp
 = do tipProgram <- readFile fp
      let productions = eval (tParse tipProgram) 1  -- 1 cause entry 
      let productions' = evalwrpr productions       -- Node has not 
      let sproductions = putids productions' 0
      let controlpoints = getctrpoints sproductions sproductions
      showctrpoints controlpoints 0

doInterval :: FilePath -> IO()
doInterval fp
 = do tipProgram <- readFile fp
      let productions = eval (tParse tipProgram) 1  -- 1 cause entry 
      let productions' = evalwrpr productions       -- Node has not 
      let sproductions = putids productions' 0
      let maxcolsize = maximum [length(pretyshow x) | (_, x) <- sproductions]
      let controlpoints = getctrpoints sproductions sproductions
      let const = getconst sproductions
      let vars = [] : iterations controlpoints [] 0 const
      showintanalysis controlpoints vars 0 maxcolsize

--------------------------------------------------------------------------------
-- IntervalAnalysis
--
--------------------------------------------------------------------------------
--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Purpose  : This module include the two main function
--             to execute the interval analysis


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
iteration::[PredCFGNode]->Int->[Int]->VarState->VarState->VarStates->VarStates->VarStates
iteration [] _ _ _ _ _ stateIn = stateIn
iteration ((EntryNode,_):nodes) current _ _ _ stateOld stateIn
   =  iteration nodes  (current+1) ((current+1):[]) [] [] stateOld [nub(getVarTop nodes)]
iteration (((AsgNode var exp),n):nodes) current reachable intersect union stateOld stateIn
   | elem current reachable
      = let 
           pastState = getUnionPredIntervals n stateOld stateIn 
           inter1 =  transformExp exp 
           inter2 =  convertVartoVal inter1 pastState
           inter3 =  evalInterExp inter2
           state = replaceVarVal pastState var inter3
        in iteration nodes  (current+1) ((current+1):reachable) [] [] stateOld (stateIn ++ [state])
    | otherwise 
      = let
           state2 = stateOld !! current
        in iteration nodes  (current+1) reachable [] [] stateOld (stateIn ++ [state2]) 
iteration (((IfGotoNode exp next),n):nodes)  current reachable intersect union stateOld stateIn
    | elem current reachable
      = let 
           pastState = getUnionPredIntervals n stateOld stateIn
           eval = evalCondition exp pastState next (current+1) reachable
           trueIntersec = fst(fst eval)
           falseIntersec = snd(fst eval)
           newReachable = snd eval
           newState = intersecVarState pastState trueIntersec
        in iteration nodes (current+1) newReachable falseIntersec pastState stateOld (stateIn ++ [newState])   
    |otherwise 
      = let 
           state = stateOld !! current
           eval = evalCondition exp state next (current+1) reachable
           falseIntersec = snd(fst eval)
        in iteration nodes (current+1) reachable falseIntersec state stateOld (stateIn ++ [state])
iteration (((GotoNode next),n):nodes) current reachable intersect union stateOld stateIn
    | elem current reachable
      = let
           pastState = getPastState union (getUnionPredIntervals n stateOld stateIn) 
           newState = intersecVarState pastState intersect 
        in iteration nodes (current+1) (next:reachable) [] [] stateOld (stateIn ++ [newState])
    | otherwise
      = let   
           state = stateOld !! current
        in
           iteration nodes (current+1) reachable [] [] stateOld (stateIn ++ [state])
iteration (((OutputNode exp),n):nodes)  current reachable intersect union stateOld stateIn
    | elem current reachable
      = let
            state = getUnionPredIntervals n stateOld stateIn
        in  iteration nodes (current+1) ((current+1):reachable) [] [] stateOld (stateIn ++ [state])
    | otherwise
       = let         
            state = stateOld !! current
         in iteration nodes  (current+1) ((current+1):reachable){--reachable --} [] [] stateOld (stateIn ++ [state])
iteration (((ExitNode),n):nodes)  current reachable intersect union stateOld stateIn
    | elem current reachable
        = let
             pastState = getUnionPredIntervals n stateOld stateIn
          in (stateIn ++ [pastState])
    | otherwise
       = let         
            state = stateOld !! current
         in (stateIn ++ [state])

getPastState::VarState->VarState->VarState
getPastState conditional predecessors=
    if (length conditional) == 0 then
        predecessors    
    else
        conditional

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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PrettyPrints
-- Present the varstate output
--------------------------------------------------------------------------------
showintanalysis :: [(CFGNode, [Pos])] -> [VarState] -> Pos -> Int -> IO()
showintanalysis [] _ n _ = do putStr "" 
showintanalysis ((node, _):xs) (y:ys) n w = do 
            let pnode = pretyshow node
            let lnode = length pnode
            putStrLn $ (show n) ++ (filler w) ++ "  | " ++ show n ++ ": "++ (showvars y)
            putStrLn $ "   " ++ pnode ++ filler (w - lnode) ++ "|    " ++ pnode
            showintanalysis xs ys (n+1) w

showvars :: VarState -> String
showvars [] = ""
showvars ((v, i):xs) = do 
            show v ++ "=" ++ show i ++ " " ++ showvars xs

filler n = replicate n ' '

