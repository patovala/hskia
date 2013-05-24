-------------------------------------------------------------------------------
--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : This module has a set of operations
--           : to do operations between varState
--           : include also the operations to evaluate 
--           : the branch and the conditions for each branch
-------------------------------------------------------------------------------

module TEvalConditions(evalConditionBorder,removeBorders)

where
import Data.List
import TCFlow(CFGNode(..),PredCFGNode)
import TInterval(Interval(..),Lb(..),Ub(..), intersec, AbsValue(..))
import qualified TInterval(union)
import TEvalInterval(InterExp(..),transformExp, evalInterExp)
import Syntax (Exp(..),Opkind(..),Stmt(..))
import TVarStateOperations(entryState, getVarBottom, VarState(..),
                           VarStates(..),getVarTop, getUnionPredIntervals, 
                           convertVartoVal,replaceVarVal,evalCondition, 
                           intersecVarState,getVarNoReachState,getVarTopState,getIntersectIntervals)



removeBorders::VarState->VarState->VarState
removeBorders [] [] = []
removeBorders [] x = x
removeBorders((s1,AInterval(Interval (Lb lb1) (Ub ub1))):vars1) 
              ((s2,AInterval(Interval (Lb lb2) (Ub ub2))):vars2)
   |(lb1 == lb2) && (ub1<ub2) = (s2,AInterval(Interval (Lb (ub1+1)) (Ub ub2))):(removeBorders vars1 vars2)
   |(ub1 == ub2) && (lb1>lb2) =  (s2,AInterval(Interval (Lb lb2) (Ub (lb1-1)))):(removeBorders vars1 vars2)
   | otherwise = (s2,AInterval(Interval (Lb lb2) (Ub ub2))):(removeBorders vars1 vars2)
removeBorders((s1,_):vars1)((s2,i):vars2) = (s2,i):(removeBorders vars1 vars2)

evalConditionBorder::Exp->VarState->Int->Int->(VarState,VarState)
evalConditionBorder exp state branchTrue branchFalse 
    = let
         interExp1 = transformExp exp
         interExp2 = convertVartoVal interExp1 state
         inter3 = evalInterExp interExp2
         interTrueFalse1 = getRightBorder inter3 interExp1 state 
         interTrueFalse2 = getLeftBorder inter3 interExp1 state 
                           interTrueFalse1 
      in interTrueFalse2

getRightBorder::Interval->InterExp->VarState->(VarState,VarState)
getRightBorder branch (MoreInter exp (VarInter s)) state
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
getRightBorder branch (EqualInter exp (VarInter s)) state
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
getRightBorder _ _ state
   = ((getVarTopState state),(getVarTopState state))

getLeftBorder::Interval->InterExp->VarState->
                 (VarState,VarState)->(VarState,VarState)
getLeftBorder branch (MoreInter (VarInter s) exp) state 
                 (interTrue1, interFalse1)
   = let exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getBorderIntervals branch 
                     (MoreInter (VarInter s) (ConInter inter1))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getLeftBorder branch (EqualInter (VarInter s) exp) state 
                (interTrue1, interFalse1)
   = let exp1 = convertVartoVal exp state
         inter1 = evalInterExp exp1
         intersect1 = getBorderIntervals branch 
                     (EqualInter (VarInter s) (ConInter inter1))
         interTrue2 = replaceVarVal interTrue1 s (snd(fst intersect1))
         interFalse2 = replaceVarVal interFalse1  s (snd(snd intersect1))
     in 
         (interTrue2,interFalse2)
getLeftBorder _ _ state (interTrue1, interFalse1)
   = (interTrue1,interFalse1)


getBorderIntervals::Interval->InterExp->((String,Interval),(String,Interval))
 
getBorderIntervals branch 
                     (MoreInter(ConInter (Interval (Lb i)(Ub i2)))
                     (VarInter s)) 
   =((s,Empty), (s,Empty))

getBorderIntervals branch (MoreInter(ConInter _)(VarInter s))
   =((s,Empty), (s,Empty))  
   
getBorderIntervals branch 
                     (EqualInter(ConInter (Interval (Lb i)(Ub i2)))
                     (VarInter s))
   =((s,Empty), (s,(Interval (Lb i)(Ub i2))))
   
getBorderIntervals branch (EqualInter (ConInter _)(VarInter s))
   =((s,Empty), (s,Empty))
   
getBorderIntervals branch (MoreInter(VarInter s) 
                     (ConInter (Interval (Lb i)(Ub i2))))
   =((s,Empty), (s,Empty))

getBorderIntervals branch (MoreInter(VarInter s) (ConInter _))
   =((s,Empty), (s,Empty))
   
getBorderIntervals branch (EqualInter(VarInter s) 
                     (ConInter (Interval (Lb i)(Ub i2))))
   =((s,Empty), (s,(Interval (Lb i)(Ub i2))))

getBorderIntervals branch (EqualInter(VarInter s) (ConInter _))
   =((s,Empty), (s,Empty))

   
