module Division
where
import TInterval(Interval(..),Lb(..),Ub(..),union,intersec)

divi::Interval->Interval->Interval
divi (Interval (Lb a) (Ub b)) (Interval (Lb c) (Ub d)) =	
      Interval (Lb (minimum [(a `div` c),(a `div` d),(b `div` c),(b `div` d)]))
       (Ub (maximum [(a `div` c),(a `div` d),(b `div` c),(b `div` d)]))
divi Empty Empty = Empty    
divi Empty x = Empty
divi x Empty = Empty
    
-- Interval
divInter::Interval->Interval-> (Interval,Interval,Interval,Interval,Interval)
divInter (Interval (Lb a) (Ub b)) (Interval (Lb c) (Ub d)) =
    let a1b1 = intersec (Interval (Lb a) (Ub b)) (Interval MinInf (Ub (-1)))
        a2b2 = intersec (Interval (Lb a) (Ub b)) (Interval (Lb 1) PlusInf)
        c1d1 = intersec (Interval (Lb c) (Ub d)) (Interval MinInf (Ub (-1)))
        c2d2 = intersec (Interval (Lb c) (Ub d)) (Interval (Lb 1) PlusInf)
        i1 = divi a1b1 c1d1
        i2 = divi a1b1 c2d2
        i3 = divi a2b2 c1d1
        i4 = divi a2b2 c2d2
        
        u1 = union i1 i2
        u2 = union u1 i3
        u3 = union u2 i4
     in (i1,i2,i3,i4,u3)

{--	(Interval MinInf  _) / (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  PlusInf) / (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  _) / (Interval MinInf _) = Interval (MinInf) (PlusInf)
    (Interval _  _) / (Interval _ PlusInf) = Interval (MinInf) (PlusInf)
    fromRational = undefined
	
    (Interval (Lb a)  (Ub b)) / (Interval (Lb 0) (Ub _)) = Empty
    (Interval (Lb a)  (Ub b)) / (Interval (Lb _) (Ub 0)) = Empty
    (Interval (Lb a)  (Ub b)) / (Interval (Lb c) (Ub d)) = 
       Interval 


	(/) Empty Empty = Empty    
    (/) Empty x = Empty
    (/) x Empty = Empty--}
