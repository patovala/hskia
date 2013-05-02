-- A super interval calculator
--
-- PV


module TInterval (Interval(..))
where

-- Interval ej: [-1, 1] or [-oo, oo] or [1,oo]
data Interval 
   = Empty 
   | Interval Lb Ub
  
-- Lower bound: -1,1,-oo
data Lb =  MinInf | Lb Int 
    deriving (Ord)
 -- deriving (Show,Eq)

-- Upper bound: -1,1,-oo
data Ub = Ub Int | PlusInf
    deriving (Ord)
 -- deriving (Show,Eq)

instance Show Lb where
    show (Lb i) = show i
    show MinInf = "-oo"

instance Show Ub where
    show (Ub i) = show i
    show PlusInf = "oo"

instance Show Interval where
    show Empty = "_|_"
    show (Interval lb ub) = "["++ show lb ++","++show ub++"]" 

instance Eq Lb where
    (==) (Lb x) (Lb y)
        | x == y = True
        | otherwise = False
    (==) MinInf MinInf = True
    (==) _ _ = False

instance Eq Ub where
    (==) (Ub x) (Ub y)
        | x == y = True
        | otherwise = False
    (==) PlusInf PlusInf = True
    (==) _ _ = False

nstance Eq Interval where
    (==) (Interval lb1 ub1) (Interval lb2 ub2)
        | lb1 == lb2 && ub1 == ub2 = True
        | otherwise = False
    (==) Empty Empty = True
    (==) _ _ = False
 
-- I think that there is no ordering (Ord) in the lattice of
-- integers intervals U _|_
-- because of the lattice of intervals U _|_
instance Ord Interval where
    compare (Interval a b) (Interval c d) 
        | a <= c && b <= d = LT
        | a > c && b > d = GT
        | otherwise = EQ

instance Num Lb where
    (+) MinInf MinInf = MinInf 
    (+) x MinInf = MinInf 
    (+) MinInf x = MinInf 
    (+) (Lb a) (Lb b) = Lb (a + b)
    (-) MinInf MinInf = MinInf 
    (-) x MinInf = MinInf 
    (-) MinInf x = MinInf 
    (-) (Lb a) (Lb b) = Lb (a - b)
    (*) MinInf MinInf = MinInf 
    (*) _ MinInf = MinInf 
    (*) MinInf _ = MinInf 
    (*) (Lb a) (Lb b) = Lb (a * b)
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Num Ub where
    (+) PlusInf PlusInf = PlusInf 
    (+) x PlusInf = x 
    (+) PlusInf x = x 
    (+) (Ub a) (Ub b) = Ub (a + b)
    (-) PlusInf PlusInf = PlusInf 
    (-) x PlusInf = PlusInf 
    (-) PlusInf x = PlusInf 
    (-) (Ub a) (Ub b) = Ub (a - b)
    (*) PlusInf PlusInf = PlusInf 
    (*) _ PlusInf = PlusInf 
    (*) PlusInf _ = PlusInf 
    (*) (Ub a) (Ub b) = Ub (a * b)
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Num Interval where
    -- (+) Empty Empty = Empty
    (+) Empty _ = Empty
    (+) _ Empty = Empty
    -- [a + c, b + d]
    (Interval a  b) + (Interval c d) = Interval (a + c) (b + d)
    (Interval a  b) - (Interval c d) = Interval (a - c) (b - d)
    Empty - _ = Empty 
    _ - Empty = Empty 
    (Interval (Lb a)  (Ub b)) * (Interval (Lb c) (Ub d)) = 
                    Interval (Lb (minimum [(a * c),(a * d),(b * c),(b * d)]))
                             (Ub (maximum [(a * c),(a * d),(b * c),(b * d)]))
    (Interval MinInf  _) * (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  PlusInf) * (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  _) * (Interval MinInf _) = Interval (MinInf) (PlusInf)
    (Interval _  _) * (Interval _ PlusInf) = Interval (MinInf) (PlusInf)
                    
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Fractional Interval where
    (/) Empty Empty = Empty    
    (/) Empty x = x
    (/) x Empty = x

    (Interval (Lb a)  (Ub b)) / (Interval (Lb 0) (Ub _)) = Empty
    (Interval (Lb a)  (Ub b)) / (Interval (Lb _) (Ub 0)) = Empty
    (Interval (Lb a)  (Ub b)) / (Interval (Lb c) (Ub d)) = 
                    Interval (Lb (minimum [(a `div` c),(a `div` d),(b `div` c),(b `div` d)]))
                             (Ub (maximum [(a `div` c),(a `div` d),(b `div` c),(b `div` d)]))
    (Interval MinInf  _) / (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  PlusInf) / (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _  _) / (Interval MinInf _) = Interval (MinInf) (PlusInf)
    (Interval _  _) / (Interval _ PlusInf) = Interval (MinInf) (PlusInf)
    fromRational = undefined

-- PV this define the union of two intervals
union :: Interval -> Interval -> Interval
union Empty x = x
union x Empty = x
union (Interval lb1 ub1) (Interval lb2 ub2) = Interval (min lb1 lb2) (max ub1 ub2)

-- PV this define the union of two intervals
intersec :: Interval -> Interval -> Interval
intersec Empty x = x
intersec x Empty = x
intersec (Interval (Lb a) (Ub b)) (Interval (Lb c) (Ub d)) 
    | b > c && a<d = Interval (Lb c) (Ub b)
    | otherwise = Empty
intersec (Interval MinInf (Ub a)) (Interval (Lb b) PlusInf)
    | a > b = Interval (Lb b) (Ub a)
    | otherwise = Empty
intersec (Interval (Lb b) PlusInf) (Interval MinInf (Ub a)) 
    | a > b = Interval (Lb b) (Ub a)
    | otherwise = Empty
intersec (Interval MinInf PlusInf) i = i 
intersec i (Interval MinInf PlusInf) = i 
intersec _ _ = Empty   

j = Interval MinInf (Ub 5)
i = Interval (Lb (-5)) PlusInf 
k = Interval (Lb (-3)) (Ub 5)

