-------------------------------------------------------------------------------
--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : This module has a set of operations
--           : to do operations between varState
--           : include also the operations to evaluate 
--           : the branch and the conditions for each branch
-------------------------------------------------------------------------------

module TVarStateOperations(VarState,VarStates, getVarTop, getUnionPredIntervals,
                            convertVartoVal, replaceVarVal, 
                            evalCondition, intersecVarState,entryState, 
                            getVarBottom,getVarNoReachState)

where
import Data.List
--import Syntax (Exp(..),Opkind(..),Stmt(..))
import TCFlow(CFGNode(..),PredCFGNode)
import TInterval(Interval(..),Lb(..),Ub(..), intersec, AbsValue(..))
import qualified TInterval(union)
import TEvalInterval(InterExp(..),transformExp, evalInterExp)
import Syntax (Exp(..),Opkind(..),Stmt(..))

--Algorithm Structures
type VarName = String
--Represent the state of the variable in 
--a node
type VarState = [(VarName, AbsValue)]
--Set with the variable value in each node
type VarStates = [VarState]


--Find all the program variables
--and return an initial state with
--all variables with bottom value   
entryState :: Int -> VarState->VarStates
entryState 0 state = []    
entryState n state = state:(entryState (n-1) state)     

-- This function return
-- the varState bottom having the tip
-- program representation in a list of
-- [PredCFGNode]
getVarBottom :: [PredCFGNode] -> VarState
getVarBottom [] = []
getVarBottom (((AsgNode var exp),_):nodes) 
   = ((var,AInterval(Empty)):(getVarBottom nodes)) 
getVarBottom (n:nodes)
   = getVarBottom nodes

-- This function return
-- the varState bottom 
-- receiving a VarState
toEmpty [] = []
toEmpty ((var,inter):vars)
   = ((var,AInterval(Empty)):(toEmpty vars))

-- This function return
-- the varState Top [-oo,oo] having the tip
-- program representation in a list of
-- [PredCFGNode]
getVarTop :: [PredCFGNode] -> VarState
getVarTop [] = []
getVarTop (((AsgNode var exp),_):nodes) 
   = ((var,AInterval(Interval MinInf PlusInf)):(getVarTop nodes)) 
getVarTop (n:nodes)
   = getVarTop nodes
-------------------------------------------------------------------------------
-- This function recibe an interExp
-- and a VarState with the values of 
-- all variables and return the expression
-- transforming variables with values
convertVartoVal::InterExp->VarState->InterExp
convertVartoVal (ConInter n) state 
   = ConInter n
convertVartoVal (VarInter s) state
   = let inter = getVarVal state s
     in ConInter inter
convertVartoVal (PlusInter e1 e2) state 
   = PlusInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal (MinusInter e1 e2) state
   = MinusInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal (MultInter e1 e2) state
   = MultInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal (DivInter e1 e2) state
   = DivInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal (MoreInter e1 e2) state
   =  MoreInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal (EqualInter e1 e2) state
   = EqualInter (convertVartoVal e1 state) (convertVartoVal e2 state) 
convertVartoVal InputInter _ 
   = InputInter
-------------------------------------------------------------------------------   
--This function receive a List of Int            		  
--The first VarStates is the state of the variables in the last iteration
--The second VarStates is the state of the varialbes in the current iteration
--It the calculation does not exist in the current operation it takes
--the value from the last operation
getUnionPredIntervals::[Int]->VarStates->VarStates->VarState
getUnionPredIntervals [] oldState currentState= toEmpty (oldState !! 1)
getUnionPredIntervals (x:xs) oldState currentState
   | x <(length currentState) =   
      let varState = currentState !! x
	  in unionVarState varState (getUnionPredIntervals xs oldState currentState)
   | otherwise =
      let varState = oldState !! x
	  in unionVarState varState (getUnionPredIntervals xs oldState currentState)

--This function intersect varState
intersecVarState::VarState->VarState->VarState
intersecVarState [] [] = []
intersecVarState v [] = v
intersecVarState [] v = v
intersecVarState ((var1,inter1):vars1)((var2,inter2):vars2)
   = (var1,(intersec inter1 inter2)):(intersecVarState vars1 vars2)
	  
--This function do union of two varState	 
unionVarState::VarState->VarState->VarState
unionVarState [] [] = []
unionVarState ((var1,inter1):vars1)((var2,inter2):vars2)
   = (var1,TInterval.union inter1 inter2):(unionVarState vars1 vars2)
    
--Function that update the state of a variable in a program point
replaceVarState::VarStates->Int->VarState->VarStates
replaceVarState (s1:states) 0 s2 
   = s2:states
replaceVarState (s1:states) i s2 
   = s1:(replaceVarState states (i-1) s2)
	
--Function that replace the value of a variable	
replaceVarVal::VarState->String->Interval->VarState
replaceVarVal [] var i 
   = [] 
replaceVarVal ((x,NoReach):state) y i2 
   | x == y = (x,(AInterval i2)):state
   | otherwise = (x,(AInterval Empty)):(replaceVarVal state y i2)	

replaceVarVal ((x,(AInterval i1)):state) y i2 
   | x == y = (x,(AInterval i2)):state
   | otherwise = (x,(AInterval i1)):(replaceVarVal state y i2)	

--Function that return the value of a variable
getVarVal::VarState->String->Interval 
getVarVal [] _ = Empty
getVarVal (( x,NoReach):state) y  
   | x == y = Empty
   | otherwise = getVarVal state y 	
getVarVal (( x,(AInterval i1)):state) y  
   | x == y = i1
   | otherwise = getVarVal state y 	
   
-----Functions to eval branchs-------------------------------
-------------------------------------------------------------------------------
--This function receive the Expression the union state
--and return the possible value of intersection for each
--branch for variable 
evalCondition::Exp->VarState->Int->Int->[Int]->((VarState,VarState),[Int])
evalCondition exp state branchTrue branchFalse reachable
    = let
         interExp1 = transformExp exp
         interExp2 = convertVartoVal interExp1 state
         inter3 = evalInterExp interExp2
         newReachable = addReachables inter3 reachable branchTrue branchFalse
         interTrueFalse1 = getRightIntersect inter3 interExp1 state 
         interTrueFalse2 = getLeftIntersect inter3 interExp1 state 
                           interTrueFalse1 
      in (interTrueFalse2,newReachable)


getRightIntersect::Interval->InterExp->VarState->(VarState,VarState)
getRightIntersect branch (MoreInter exp (VarInter s)) state
   = let interTrue1 = getVarTopState state
         interFalse1 = getVarTopState state
         exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getIntersectIntervals branch 
                     (MoreInter (ConInter inter1) (VarInter s))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getRightIntersect branch (EqualInter exp (VarInter s)) state
   = let interTrue1 = getVarTopState state
         interFalse1 = getVarTopState state
         exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getIntersectIntervals branch 
                     (EqualInter (ConInter inter1) (VarInter s))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getRightIntersect _ _ state
   = ((getVarTopState state),(getVarTopState state))

getLeftIntersect::Interval->InterExp->VarState->
                 (VarState,VarState)->(VarState,VarState)
getLeftIntersect branch (MoreInter (VarInter s) exp) state 
                 (interTrue1, interFalse1)
   = let exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getIntersectIntervals branch 
                     (MoreInter (VarInter s) (ConInter inter1))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getLeftIntersect branch (EqualInter (VarInter s) exp) state 
                (interTrue1, interFalse1)
   = let exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getIntersectIntervals branch 
                     (EqualInter (VarInter s) (ConInter inter1))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getLeftIntersect _ _ state (interTrue1, interFalse1)
   = (interTrue1,interFalse1)

--Funtion that receive a more interval expresion
--and return a tuple that its first value is the
--interval for true condition and the second value
--is the interval for false condition  
getIntersectIntervals::Interval->InterExp->
                       ((String,Interval),(String,Interval)) 
getIntersectIntervals branch 
                     (MoreInter(ConInter (Interval (Lb i)(Ub i2)))
                     (VarInter s))
    | branch == Interval(Lb 1) (Ub 1) =
       ((s,Interval MinInf (Ub (i-1))), (s,Interval (Lb i) PlusInf))
    | branch == Interval(Lb 0) (Ub 0) =
       ((s,Interval MinInf (Ub (i))), (s,Interval (Lb i) PlusInf))
    | branch == Interval MinInf PlusInf =
       ((s,Interval MinInf (Ub (i-1))), (s,Interval (Lb i) PlusInf))

getIntersectIntervals branch (MoreInter(ConInter _)(VarInter s))
   =((s,Interval MinInf PlusInf), (s,Interval MinInf PlusInf))  
   
getIntersectIntervals branch 
                     (EqualInter(ConInter (Interval (Lb i)(Ub i2)))
                     (VarInter s))
   =((s,(Interval (Lb i)(Ub i2))), (s,Interval MinInf PlusInf))
   
getIntersectIntervals branch (EqualInter (ConInter _)(VarInter s))
   =((s,Interval MinInf PlusInf), (s,Interval MinInf PlusInf))
   
getIntersectIntervals branch (MoreInter(VarInter s) 
                     (ConInter (Interval (Lb i)(Ub i2))))
    | branch == Interval(Lb 1) (Ub 1) =
        ((s,Interval (Lb (i+1)) PlusInf), (s,Interval MinInf (Ub i)))
    | branch == Interval(Lb 0) (Ub 0) =
        ((s,Interval (Lb (i)) PlusInf), (s,Interval MinInf (Ub i)))
    | branch == Interval MinInf PlusInf =
        ((s,Interval (Lb (i+1)) PlusInf), (s,Interval MinInf (Ub i)))

getIntersectIntervals branch (MoreInter(VarInter s) (ConInter _))
   =((s,Interval MinInf PlusInf), (s,Interval MinInf PlusInf))
   
getIntersectIntervals branch (EqualInter(VarInter s) 
                     (ConInter (Interval (Lb i)(Ub i2))))
   =((s,(Interval (Lb i)(Ub i2))), (s,Interval MinInf PlusInf))

getIntersectIntervals branch (EqualInter(VarInter s) (ConInter _))
   =((s,Interval MinInf PlusInf), (s,Interval MinInf PlusInf))
   
--Function that add the reachbles according
--with the value of an interval   
addReachables::Interval->[Int]->Int->Int->[Int]
addReachables (Interval (Lb 1) (Ub 1)) reachable yes no 
   = (yes:reachable)
addReachables (Interval (Lb 0) (Ub 0)) reachable yes no 
   = (no:reachable)
addReachables (Interval  MinInf  PlusInf) reachable yes no 
   = (yes:no:reachable)

getVarTopState::VarState->VarState
getVarTopState [] = []
getVarTopState ((s, i):states) = 
   (s,AInterval(Interval MinInf PlusInf)):(getVarTopState states)

getVarNoReachState::VarState->VarState
getVarNoReachState (states) =
   let  f1 = \(v1,i1) -> i1 == AInterval(Empty)
        f2 = \(v2,i2) -> (v2,NoReach)
        list = map f2 states
   in
        if (filter f1 states) == states then   
            list
        else
            states

{--initMap :: [DFNode] -> [(DFNode, [Mapping], [Mapping])]
initMap [] = []
initMap nodes = initMap' nodes varsBot
    where varsBot = sortMapping (map f allVars)   -- Map all vars to bot
          allVars = getVars nodes                 -- Get all vars
          f = \s -> s = NoReach --}
