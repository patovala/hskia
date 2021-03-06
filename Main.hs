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

import TCFlow(flowFile, evalwrpr, eval, CFGNode(..), PredCFGNode(..), 
              pretyshow)
import TParse (tParse, pretty, parseFile)
import TControl (showctrpoints, getctrpoints, putids, Pos, getconst
                ,spprint , spprint2)
import TInterval(Interval(..),Lb(..),Ub(..), AbsValue(..),intersec)
import System.Environment (getProgName, getArgs)
import TEvalInterval (InterExp(..),transformExp, evalInterExp)
import Data.List
import Syntax (Exp(..),Opkind(..),Stmt(..))
import TVarStateOperations(entryState, getVarBottom, VarState(..),
                           VarStates(..),getVarTop, getUnionPredIntervals, 
                           convertVartoVal,replaceVarVal,evalCondition, 
                           intersecVarState,getVarNoReachState)
import TEvalConditions(evalConditionBorder,removeBorders)
import TOptimizer(removedead)

help :: String -> IO ()
help tipprog 
  = do
    putStrLn ("Usage: " ++ tipprog ++ " [-a -p -i -o] filename\n")
    putStrLn ("      -a: get the production list from file\n ")
    putStrLn ("      -p: get the predecesors list from file\n ")
    putStrLn ("      -i: start the interval\n ")
    putStrLn ("      -o: optimize code\n")

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
            "-o" -> doOptimize inputFile 
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
      let maxcolsize = maximum [length(show n ++ pretyshow x) | 
                                (n, x) <- sproductions] + 8
      let controlpoints = getctrpoints sproductions sproductions
      let const = getconst sproductions
      let vars =  iterations controlpoints [] 0 0 const
      showintanalysis controlpoints vars 0 maxcolsize

doOptimize :: FilePath -> IO()
doOptimize fp
 = do tipProgram <- readFile fp
      let productions = eval (tParse tipProgram) 1  -- 1 cause entry 
      let productions' = evalwrpr productions       -- Node has not 
      let sproductions = putids productions' 0
      let controlpoints = getctrpoints sproductions sproductions
      let const = getconst sproductions
      let vars = iterations controlpoints [] 0 0 const
      let fixedctrpoints = removedead sproductions vars
      spprint2 sproductions fixedctrpoints 
      --spprint fixedctrpoints 

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
iterations::[PredCFGNode]->VarStates->Int->Int->[Int]->VarStates
iterations nodes x 0 0 lmarks
   = let initialIteration = iteration nodes 0 lmarks [] [] [] 
                            (entryState (length nodes) 
                            (nub(getVarBottom nodes))) []
         secondIteration = iteration nodes 0 lmarks [] [] [] initialIteration []
         thirdIteration = iteration nodes 0 lmarks [] [] [] secondIteration []
     in  iterations nodes initialIteration 1 0 lmarks 

iterations nodes stateIn i 4 lmarks = stateIn 

iterations nodes stateIn i j lmarks
   |i == 3 = 
      let newState = iteration nodes 0 lmarks [] [] [] stateIn []
          stateWidening = wideningState stateIn newState lmarks
      in
          iterations nodes stateWidening 1 (j+1) lmarks 
          --stateWidening
   |otherwise =
     let newState = iteration nodes  0 lmarks [] [] [] stateIn []
     in 
        if(stateIn == newState) then
           newState
        else
           iterations nodes newState (i+1) j lmarks

--This Algorithm assign the value of the variable evaluating each node 
--The parameters are:
--nodes: the list of node that represent a tip program
--current: the id of the current node
--reachables: List of the nodes that are reachable in the 
--            current iteration
--intersect: It is the intersect of the false branch evaluated
--           in a conditional node 
--stateOld : The state of the previous iterantion
--stateIn : where the state of the current iteration is build
-------------------------------------------------------------------------------
iteration::[PredCFGNode]->Int->[Int]->VarState->VarState->VarState->
           VarStates->VarStates->VarStates
iteration [] _ _ _ _ _ _ stateIn = stateIn
iteration ((EntryNode,_):nodes) current _ _ _ _ stateOld stateIn
   =  iteration nodes  (current+1) ((current+1):[]) 
                        --[] [] stateOld [nub(getVarTop nodes)]
                        [] [] [] stateOld [nub(getVarBottom nodes)]
iteration (((AsgNode var exp),n):nodes) current reachable vintersect 
          vunion vborders stateOld stateIn
   | elem current reachable
      = let 
           pastState1 = getUnionPredIntervals n stateOld stateIn 
           pastState = map changeNoReach pastState1
           inter1 =  transformExp exp 
           inter2 =  convertVartoVal inter1 pastState
           inter3 =  evalInterExp inter2
           state = replaceVarVal pastState var inter3
        
        in iteration nodes  (current+1) ((current+1):reachable) 
           [] [] [] stateOld (stateIn ++ [state])
    | otherwise 
      = let
           state2 = getVarNoReachState(stateOld !! current)
        in iteration nodes  (current+1) reachable [] [] []
           stateOld (stateIn ++ [state2]) 
iteration (((IfGotoNode exp next),n):nodes)  current reachable 
          vintersect vunion vborders stateOld stateIn
    | elem current reachable
      = let 
           pastState1 = getUnionPredIntervals n stateOld stateIn 
           pastState = map changeNoReach pastState1
           eval = evalCondition exp pastState next (current+1) reachable
           evalBorders = evalConditionBorder exp pastState next (current+1) 
           falseBorders = snd evalBorders
           trueIntersec = fst(fst eval)
           falseIntersec = snd(fst eval)
           newReachable = snd eval
           preNewState = intersecVarState pastState trueIntersec
           newState = changeBottom preNewState pastState
        in iteration nodes (current+1) newReachable falseIntersec 
           pastState1 falseBorders stateOld (stateIn ++ [newState])   
           -- error (show pastState1)
    |otherwise 
      = let 
           state = getVarNoReachState (stateOld !! current)
           eval = evalCondition exp state next (current+1) reachable
           evalBorders = evalConditionBorder exp state next (current+1) 
           falseBorders = snd evalBorders
           falseIntersec = snd(fst eval)      
        in iteration nodes (current+1) reachable falseIntersec 
            state falseBorders  stateOld (stateIn ++ [state])
iteration (((GotoNode next),n):nodes) current reachable vintersect 
          vunion vborders stateOld stateIn
    | elem current reachable
      = let
         pastState1 = getPastState vunion 
                     (getUnionPredIntervals n stateOld stateIn) 
         pastState = map changeNoReach pastState1
         newBorders = removeBorders vborders pastState  
         newState = intersecVarState newBorders vintersect
         
        in iteration nodes (current+1) (next:reachable) 
           [] [] [] stateOld (stateIn ++ [newState])
           -- error (show vunion)
    | otherwise
      = let   
           state = getVarNoReachState (stateOld !! current)
        in
           -- error (show vunion)
           iteration nodes (current+1) reachable [] [] []
           stateOld (stateIn ++ [state])
iteration (((OutputNode exp),n):nodes)  current reachable 
          vintersect vunion vborders stateOld stateIn
    | elem current reachable
      = let
           state1 = getUnionPredIntervals n stateOld stateIn
           state = map changeNoReach state1
        in  iteration nodes (current+1) ((current+1):reachable) [] [] []
            stateOld (stateIn ++ [state])
    | otherwise
       = let         
            state = getVarNoReachState (stateOld !! current)
         in iteration nodes  (current+1) reachable [] [] []
            stateOld (stateIn ++ [state])
iteration (((ExitNode),n):nodes)  current reachable vintersect 
          vunion vborders stateOld stateIn
    | elem current reachable
        = let
             pastState1 = getUnionPredIntervals n stateOld stateIn
             pastState = map changeNoReach pastState1
          in (stateIn ++ [pastState])
    | otherwise
       = let         
            state = getVarNoReachState (stateOld !! current)
         in (stateIn ++ [state])

changeBottom::VarState->VarState->VarState
changeBottom [] [] =[]
changeBottom ((var1,NoReach):vars1)((var2,inter2):vars2)
   = (var1,(inter2)):(changeBottom vars1 vars2)
changeBottom ((var1,AInterval Empty):vars1)((var2,inter2):vars2)
   = (var1,(inter2)):(changeBottom vars1 vars2)
changeBottom ((var1,AInterval inter1):vars1)((var2,AInterval inter2):vars2)
   = (var1,AInterval(inter1)):(changeBottom vars1 vars2)

changeNoReach::(String, AbsValue)->(String, AbsValue)
changeNoReach (var,NoReach) =(var,AInterval Empty)
changeNoReach (var,i) =(var,i)

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

--Widening of intervals
wideningInterval::AbsValue->AbsValue->[Int]->AbsValue

wideningInterval NoReach NoReach _ = NoReach

wideningInterval NoReach a _ = a

wideningInterval a NoReach  _ = a

wideningInterval (AInterval Empty) (AInterval Empty) _ = AInterval Empty

wideningInterval (AInterval Empty) a _ = a
   
wideningInterval a (AInterval Empty) _ = (AInterval Empty)
   
wideningInterval (AInterval(Interval lb1 ub1)) (AInterval(Interval lb2 ub2)) []
   |lb1 <= lb2 =
      if (ub1>=ub2) then
         AInterval(Interval lb1 ub1)
      else
         AInterval(Interval lb1 PlusInf)
   |otherwise =
      if (ub1>=ub2) then
         AInterval(Interval MinInf ub1)
      else
         AInterval(Interval MinInf PlusInf)

wideningInterval (AInterval(Interval lb1 ub1)) (AInterval(Interval lb2 ub2)) list
   |lb1 <= lb2 =
      if (ub1>=ub2) then
         AInterval(Interval lb1 ub1)
      else
         AInterval(Interval lb1 (minimumInterval (removeSmaller ub2 list)))
   |otherwise =
      if (ub1>=ub2) then
         AInterval(Interval (maximumInterval(removeBigger lb2 list)) ub1)
      else
         AInterval(Interval (maximumInterval(removeBigger lb2 list)) 
                  (minimumInterval (removeSmaller ub2 list)))


maximumInterval::[Int]->Lb
maximumInterval [] = MinInf
maximumInterval list= Lb (maximum list) 

minimumInterval::[Int]->Ub
minimumInterval [] = PlusInf
minimumInterval list= Ub (minimum list)

removeSmaller::Ub -> [Int] -> [Int]
removeSmaller PlusInf list = list 
removeSmaller _ []
   = []
removeSmaller (Ub y) (x:xs) 
   = if(y>x) then
        removeSmaller (Ub y) xs
     else
        x:(removeSmaller (Ub y) xs)

removeBigger::Lb->[Int]->[Int]
removeBigger MinInf list = list
removeBigger _ [] = []
removeBigger (Lb y) (x:xs) 
   = if(y<x) then
        removeBigger (Lb y) xs
     else
        x:(removeBigger (Lb y) xs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- PrettyPrints
-- Present the varstate output
-------------------------------------------------------------------------------
showintanalysis :: [(CFGNode, [Pos])] -> [VarState] -> Pos -> Int -> IO()
showintanalysis [] _ n _ = do putStr "" 
showintanalysis ((node, _):xs) (y:ys) n w = do 
            let pnode = show n ++ ":" ++ pretyshow node
            let lnode = length (pnode) 
            let vnode = show n 
            let lvnode = w - length vnode + 1
            putStrLn $ (show n) ++ (filler lvnode) ++ "  | " ++ show n ++ ": "++ (showvars y)
            putStrLn $ "   " ++ pnode ++ filler (w - lnode) ++ "|    " ++ pnode
            showintanalysis xs ys (n+1) w

showvars :: VarState -> String
showvars [] = ""
showvars ((v, AInterval i):xs) = do 
            show v ++ "=" ++ show i ++ " " ++ showvars xs
showvars ((v, NoReach):xs) = do 
            show v ++ "=" ++ "NR" ++ " " ++ showvars xs

filler n = replicate n ' '
