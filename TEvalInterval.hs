--  File     : Stateguess.hs
--  Author   : Diana Barreto - Patricio Valero
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : This module transfor arithmetic expressions 
--             to interval exp and evaluate interval expressions
module TEvalInterval (InterExp(..),transformExp, evalInterExp)

where
import Data.List
import Syntax (Exp(..),Opkind(..),Stmt(..))
import TInterval(Interval(..),Lb(..),Ub(..), intersec)
import qualified TInterval(union)

--Representation of interval expressions
data InterExp
   = ConInter Interval
   | VarInter String
   | PlusInter InterExp InterExp
   | MinusInter InterExp InterExp
   | MultInter InterExp InterExp
   | DivInter InterExp InterExp
   | MoreInter InterExp InterExp
   | EqualInter InterExp InterExp
   | InputInter  
   | UnionInter InterExp InterExp
   | IntersectInter InterExp InterExp   
   deriving (Show,Eq)
   
--Transfor aritmethic Tip Expresion to
-- an Interval Expression
transformExp::Exp->InterExp
transformExp (Con n) 
   = ConInter (Interval (Lb n) (Ub n))
transformExp (Var s) 
   = VarInter s
transformExp (Op Plus e1 e2) 
   = PlusInter (transformExp e1) (transformExp e2) 
transformExp (Op Minus e1 e2) 
   = MinusInter (transformExp e1) (transformExp e2) 
transformExp (Op Mult e1 e2) 
   = MultInter (transformExp e1) (transformExp e2) 
transformExp (Op Div e1 e2) 
   = DivInter (transformExp e1) (transformExp e2) 
transformExp (Op More e1 e2) 
   =  transformExpMore (Op More e1 e2) 
transformExp (Op Equal e1 e2) 
   = EqualInter (transformExp e1) (transformExp e2) 
transformExp Input 
   = InputInter

transformExpMore::Exp->InterExp
transformExpMore (Op More(Con i)(Var s)) 
   = MoreInter (ConInter (Interval (Lb i)(Ub i))) (VarInter s)  
transformExpMore (Op More (Var s)(Con i)) 
   = MoreInter (VarInter s) (ConInter(Interval (Lb i)(Ub i)))
transformExpMore (Op More (Var s1)(Var s2)) 
   = MoreInter (VarInter s1) (VarInter s2)
transformExpMore (Op More e1 e2) 
   = MoreInter (transformExpMore e1) (transformExpMore e2) 
transformExpMore (Op Plus e1 e2) 
   = PlusInter (transformExpMore e1) (transformExpMore e2) 
transformExpMore (Op Minus e1 e2) 
   = MinusInter (transformExpMore e1) (transformExpMore e2) 
transformExpMore (Op Mult e1 e2) 
   = MultInter (transformExpMore e1) (transformExpMore e2) 
transformExpMore (Op Div e1 e2) 
   = DivInter (transformExpMore e1) (transformExpMore e2) 
transformExpMore (Op Equal e1 e2) 
   = EqualInter (transformExp e1) (transformExp e2) 
transformExpMore Input 
   = InputInter

-------------------------------------------------------------   
--Evaluation of Interval Expressions

--Function that eval the Interval Expression
--When it is a equal or more expression
--if it is true for all interval values return [1,1] 
--if it is false for all interval values return [0,0]
--other case it return [-oo,oo] 
evalInterExp::InterExp->Interval
evalInterExp (ConInter i) 
   = i
evalInterExp (VarInter s) 
   = Empty
evalInterExp (PlusInter e1 e2) 
   = (evalInterExp e1) + (evalInterExp e2)
evalInterExp (MinusInter e1 e2) 
   = (evalInterExp e1) - (evalInterExp e2)
evalInterExp (MultInter e1 e2) 
   = (evalInterExp e1) * (evalInterExp e2) 
evalInterExp (DivInter e1 e2) 
   = (evalInterExp e1) / (evalInterExp e2)
evalInterExp (MoreInter e1 e2) 
   = let a = evalInterExp e1
         b = evalInterExp e2
     in
     if( (a > b) == False) && ((b > a) == False) then
        Interval MinInf PlusInf
     else
        if(a>b) then
           Interval (Lb 1) (Ub 1)
        else
           Interval (Lb 0) (Ub 0)
evalInterExp (EqualInter e1 e2)  
   = let a = evalInterExp e1
         b = evalInterExp e2
         c = intersec a b
     in
     if( a == b) then
        Interval (Lb 1) (Ub 1)
     else
        if(c == Empty) then
           Interval (Lb 0) (Ub 0)
        else
           Interval MinInf PlusInf
evalInterExp InputInter
  = Interval MinInf PlusInf


   
