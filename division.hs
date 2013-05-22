module Division
where
import TInterval(Interval(..),Lb(..),Ub(..),union,intersec, AbsValue(..))
import TInterval(interTest, ivs)
data SInt = SMinInf | SInt Int | SPlusInf 
    deriving (Show)
    
divcross :: Interval -> Interval -> Interval
divcross (Interval a b) (Interval c d) = Interval (tolb x) (toub y)
    where 
        (a', b',c',d') = (translb a, transub b, translb c, transub d)
        x = divSint a' d'
        y = divSint b' c'
        tolb SMinInf = MinInf 
        tolb (SInt n) = Lb n 
        tolb nn = error $ "error lb:" ++ show y
        toub SPlusInf = PlusInf 
        toub (SInt n) = Ub n 
        toub nn = error $ "error ub:" ++ show b'++"  " ++ show b 
        

divSint :: SInt -> SInt -> SInt
divSint SMinInf (SInt y) = SMinInf 
divSint (SInt x) SMinInf  = SInt 0 
-- Int
divSint (SInt x) (SInt y) 
            | (x*y) >= 0 = SInt (x `div` y) 
            | otherwise = SInt (x `quot` y) 
divSint (SInt x) SPlusInf = SInt 0
divSint SPlusInf (SInt y) = SPlusInf 
divSint _ _ = error "not allowed this divSint"

translb::Lb -> SInt 
translb (Lb x) = SInt x
translb _ = SMinInf

transub::Ub -> SInt 
transub (Ub x) = SInt x
transub _ = SPlusInf


-- Interval
divInter :: Interval -> Interval -> Interval
divInter ab cd = u
    where 
        (AInterval a1b1) = intersec (AInterval ab) 
                                    (AInterval (Interval MinInf (Ub (0))))
        (AInterval a2b2) = intersec (AInterval ab) 
                                    (AInterval (Interval (Lb 0) PlusInf))
        (AInterval c1d1) = intersec (AInterval cd) 
                                    (AInterval (Interval MinInf (Ub (-1))))
        (AInterval c2d2) = intersec (AInterval cd) 
                                    (AInterval (Interval (Lb 1) PlusInf))
        i1 = divcross a1b1 c1d1 
        -- eg: [-oo, -3] -> [3,oo]
        i2 = tonegative $ divcross (topositive a1b1) c2d2
        i3 = tonegative $ divcross a2b2 (topositive c1d1) 
        i4 = divcross a2b2 c2d2
        (AInterval u) = foldr (union . AInterval) (AInterval i1) [i2,i3,i4] 

-- transform an interval from negative to positive
-- the (-) sign is implicit for now 
topositive :: Interval -> Interval
topositive (Interval MinInf (Ub b)) = Interval (Lb b) PlusInf 
topositive (Interval (Lb a) (Ub b)) = Interval (Lb b) (Ub a) 

-- transform an interval from negative to positive
-- the (-) sign is implicit for now 
tonegative :: Interval -> Interval
tonegative (Interval (Lb a) PlusInf) = Interval MinInf (Ub a) 
tonegative (Interval (Lb a) (Ub b)) = Interval (Lb b) (Ub a) 

-- test cases: TODO DELETE
a = Interval MinInf (Ub 5)
b = Interval (Lb (-5)) PlusInf 
