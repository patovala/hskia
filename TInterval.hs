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

instance Eq Interval where
    (==) (Interval lb1 ub1) (Interval lb2 ub2)
        | lb1 == lb2 && ub1 == ub2 = True
        | otherwise = False
    (==) Empty Empty = True
    (==) _ _ = False
 
-- I think that there is no ordering (Ord) in the lattice of
-- integers intervals U _|_
-- because of the lattice of intervals U _|_
-- instance Ord Interval where
--     compare (Interval lb1 ub1) (Interval lb2 ub2)
--         | lb1 <= lb2 && ub1 <= ub2 = LT
--         | lb1 < lb2 && ub1 > ub2 = Empty
--         | otherwise = GT

instance Num Lb where
    (+) MinInf MinInf = MinInf 
    (+) x MinInf = x 
    (+) MinInf x = x 
    (+) (Lb a) (Lb b) = Lb (a + b)
    (-) MinInf MinInf = MinInf 
    (-) x MinInf = x 
    (-) MinInf x = x 
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
    (-) x PlusInf = x 
    (-) PlusInf x = x 
    (-) (Ub a) (Ub b) = Ub (a - b)
    (*) PlusInf PlusInf = PlusInf 
    (*) _ PlusInf = PlusInf 
    (*) PlusInf _ = PlusInf 
    (*) (Ub a) (Ub b) = Ub (a * b)
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Num Interval where
    (+) Empty Empty = Empty
    (+) Empty x = x
    (+) x Empty = x
    -- [a + c, b + d]
    (Interval a  b) + (Interval c d) = Interval (a + c) (b + d)
    (Interval a  b) - (Interval c d) = Interval (a - c) (b - d)
    (Interval a  b) * (Interval c d) = Interval (a * c) (b * d)
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined
    


-- PV this define the union of two intervals
-- union :: Interval -> Interval -> Interval
-- union Empty x = x
-- union x Empty = x
-- union (Interval lb1 ub2) (Interval lb2 ub2) = 

j = Interval MinInf PlusInf
i = Interval (Lb (-1)) (Ub 0)
