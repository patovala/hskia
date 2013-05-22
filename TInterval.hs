--  Author   : Diana Barreto - Ivan Patricio Valarezo
--  Id       : 574386 - 601099
--  Origin   : 06-May-2013
--  Purpose  : Implementation of Interval Operations

module TInterval (Interval(..),Lb(..),Ub(..), AbsValue(..), union, intersec, interTest, ivs)
where
import Data.List(sort)

data AbsValue
   = NoReach 
   | AInterval {interval :: Interval}
   deriving (Show,Eq)
   
-- Interval ej: [-1, 1] or [-oo, oo] or [1,oo]
data Interval 
   = Empty 
   | Interval Lb Ub

-- Lower bound: -1,1,-oo
data Lb =  MinInf | Lb {valueLb :: Int} 
    deriving (Ord)

-- Upper bound: -1,1,-oo
data Ub = Ub {valueUb :: Int} | PlusInf
    deriving (Ord)

-- Define a linear interval type to operate with
data SInt = SMinInf | SInt Int | SPlusInf 
    deriving (Show)

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
instance Ord Interval where
    compare (Interval a b) (Interval c d) 
        | a <= c && b <= d = LT
        | a > c && b > d = GT
        | otherwise = EQ
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare _ Empty = GT

instance Num Lb where
    (+) MinInf MinInf = MinInf 
    (+) x MinInf = MinInf 
    (+) MinInf x = MinInf 
    (+) (Lb a) (Lb b) = Lb (a + b)
    (-) MinInf MinInf = MinInf 
    (-) x MinInf = MinInf 
    (-) MinInf x = MinInf 
--    (-) (Lb a) (Ub b) = Lb (a - b)
    (*) MinInf MinInf = MinInf 
    (*) _ MinInf = MinInf 
    (*) MinInf _ = MinInf 
    (*) (Lb a) (Lb b) = Lb (a * b)
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Num Ub where
    (+) PlusInf PlusInf = PlusInf 
    (+) x PlusInf = PlusInf 
    (+) PlusInf x = PlusInf 
    (+) (Ub a) (Ub b) = Ub (a + b)
    (-) PlusInf PlusInf = PlusInf 
    (-) x PlusInf = PlusInf 
    (-) PlusInf x = PlusInf 
--    (-) (Ub a) (Lb b) = Ub (a - b)
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
    (Interval (Lb a) (Ub b)) - (Interval (Lb c) (Ub d)) 
            = Interval (Lb(a - d)) (Ub (b - c))
    (Interval MinInf (Ub b)) - (Interval (Lb c) _) 
            = Interval (MinInf) (Ub (b - c))
    (Interval (Lb a) PlusInf) - (Interval _ (Ub d)) 
            = Interval (Lb(a - d)) (PlusInf)
    (Interval (Lb a) (Ub b)) - (Interval MinInf (Ub d)) 
            = Interval (MinInf) (PlusInf)
    (Interval (Lb a) (Ub b)) - (Interval (Lb c) PlusInf) 
            = Interval (MinInf) (PlusInf)
    (Interval MinInf PlusInf) - (Interval _ _) = Interval (MinInf) (PlusInf)
    (Interval _ _) - (Interval MinInf PlusInf) = Interval (MinInf) (PlusInf)
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
    fromRational = undefined
    (/) Empty Empty = Empty    
    (/) Empty x = Empty
    (/) x Empty = Empty

    (Interval (Lb a)  (Ub b)) / (Interval (Lb 0) (Ub _)) = Empty
    (Interval (Lb a)  (Ub b)) / (Interval (Lb _) (Ub 0)) = Empty
    --(Interval (Lb a)  (Ub b)) / (Interval (Lb c) (Ub d)) = 
    --     Interval (Lb (minimum [(a `div` c),(a `div` d),
    --                                        (b `div` c),(b `div` d)]))
    --              (Ub (maximum [(a `div` c),(a `div` d),
    --                                        (b `div` c),(b `div` d)]))
    --(Interval MinInf  _) / (Interval _ _) = Interval (MinInf) (PlusInf)
    --(Interval _  PlusInf) / (Interval _ _) = Interval (MinInf) (PlusInf)
    --(Interval _  _) / (Interval MinInf _) = Interval (MinInf) (PlusInf)
    --(Interval _  _) / (Interval _ PlusInf) = Interval (MinInf) (PlusInf)
    (/) ab cd = u
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
topositive Empty = Empty 

-- transform an interval from negative to positive
-- the (-) sign is implicit for now 
tonegative :: Interval -> Interval
tonegative (Interval (Lb a) PlusInf) = Interval MinInf (Ub a) 
tonegative (Interval (Lb a) (Ub b)) = Interval (Lb b) (Ub a) 
tonegative Empty = Empty 

-- operates semi intervals for division
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
divcross _ _ = Empty 
--divcross m n = error $ "what is this: " ++ show m ++" " ++ show n

-- Transform from interval to serialized intervals
translb::Lb -> SInt 
translb (Lb x) = SInt x
translb _ = SMinInf

transub::Ub -> SInt 
transub (Ub x) = SInt x
transub _ = SPlusInf

-- Operate (divide) serialized intervals
divSint :: SInt -> SInt -> SInt
divSint SMinInf (SInt y) = SMinInf 
divSint (SInt x) SMinInf  = SInt 0 
-- Int
divSint (SInt x) (SInt y) 
            | (x*y) >= 0 = SInt (ceiling $ ((toRational x) / (toRational y)))
            | otherwise = SInt (x `div` y) 
divSint (SInt x) SPlusInf = SInt 0
divSint SPlusInf (SInt y) = SPlusInf 
divSint _ _ = error "this operation is undefined divSint"

-- PV this define the union of two intervals
union :: AbsValue -> AbsValue -> AbsValue
union (AInterval Empty) (AInterval Empty) = (AInterval Empty)
union (AInterval Empty) x = x
union x (AInterval Empty) = x

union NoReach NoReach = NoReach
union NoReach x = x
union x NoReach = x

union (AInterval (Interval lb1 ub1)) (AInterval(Interval lb2 ub2)) = 
   (AInterval(Interval (min lb1 lb2) (max ub1 ub2)))

-- PV this define the intersection of two intervals
intersec :: AbsValue -> AbsValue -> AbsValue
intersec (AInterval Empty)(AInterval Empty) = (AInterval Empty)
intersec (AInterval Empty) x = (AInterval Empty)
intersec x (AInterval Empty) = (AInterval Empty) 
intersec NoReach NoReach = NoReach
intersec NoReach x = NoReach
intersec x NoReach = NoReach

intersec (AInterval (Interval (Lb a) (Ub b))) 
         (AInterval (Interval (Lb c) (Ub d)))  
    | b >= c   = (AInterval(Interval (Lb b') (Ub c')))
    | otherwise  = (AInterval Empty)
        where (_:b':c':_) = sort([a,b,c,d])
        
intersec (AInterval(Interval MinInf PlusInf)) i = i 
intersec i (AInterval (Interval MinInf PlusInf)) = i 
-- new rules for infinites
-- both lb with -oo
intersec (AInterval(Interval lb1 ub1)) (AInterval (Interval lb2 ub2))
    | lb1 == MinInf && lb2 == MinInf = (AInterval (Interval MinInf ub))
                where ub = if (ub1 >= ub2) then ub2 else ub1
-- both ub with oo
intersec (AInterval (Interval lb1 ub1)) (AInterval (Interval lb2 ub2))
    | ub1 == PlusInf && ub2 == PlusInf = (AInterval (Interval lb PlusInf))
                where lb = if (lb1 >= lb2) then lb1 else lb2
-- lb1 == -oo and ub2 == oo ub1 can't be oo since it was previously catched
intersec (AInterval(Interval lb1 ub1)) (AInterval(Interval lb2 ub2))
    | lb1 == MinInf && ub2 == PlusInf = (AInterval r)
        where (Lb x) = lb2
              (Ub y) = ub1
              r = case (compare x y) of
                        EQ -> Empty
                        GT -> Empty 
                        LT -> (Interval (Lb (min x y)) (Ub (max x y)))

-- ub1 == oo and lb2 == -oo ub2 can't be oo since it was previously catched
intersec (AInterval(Interval lb1 ub1)) (AInterval(Interval lb2 ub2))
    | lb2 == MinInf && ub1 == PlusInf = (AInterval r)
        where (Lb x) = lb1
              (Ub y) = ub2
              r = case (compare x y) of
                        EQ -> Empty
                        GT -> Empty
                        LT -> Interval (Lb x) (Ub y)
-- [-oo,a] [b,c]
intersec (AInterval (Interval MinInf ub1)) (AInterval (Interval lb2 ub2)) 
    = (AInterval r)
        where
            (Ub x) = ub1
            (Lb y) = lb2
            (Ub z) = ub2
            -- IPV because intervals seems to be inclusive
            -- r = if (x<=y && x<=z) then Empty else (Interval (Lb x') (Ub y'))
            r = if (x<y && x<z) then Empty else (Interval (Lb x') (Ub y'))
            (x':y':z':[]) = sort (x:y:z:[])
-- [a,oo] [b,c]
intersec (AInterval(Interval lb1 PlusInf)) (AInterval(Interval lb2 ub2)) 
    = (AInterval r)
        where
            (Lb x) = lb1
            (Lb y) = lb2
            (Ub z) = ub2
            r = if (x>y && x>z) then Empty else (Interval (Lb y') (Ub z'))
            (x':y':z':[]) = Data.List.sort (x:y:z:[])
-- [a,b] [-oo,c]
intersec (AInterval(Interval lb1 ub1)) (AInterval(Interval MinInf ub2)) 
    = (AInterval r)
        where
            (Lb x) = lb1
            (Ub y) = ub1
            (Ub z) = ub2
            r = if (z<x && z<y) then Empty else (Interval (Lb x') (Ub y'))
            (x':y':z':[]) = sort (x:y:z:[])
-- [a,b] [c,oo]
intersec (AInterval (Interval lb1 ub1)) (AInterval(Interval lb2 PlusInf)) 
    = (AInterval r)
        where
            (Lb x) = lb1
            (Ub y) = ub1
            (Lb z) = lb2
            r = if (z>x && z>y) then Empty else (Interval (Lb y') (Ub z'))
            (x':y':z':[]) = sort (x:y:z:[])

-- Test the interval behaviour
a = [(MinInf),(Lb (-1)),(Lb 0),(Lb 2),(Lb 4)]
b = [(Ub 0),(Ub 2),(Ub 4),PlusInf]

ivs = [(x,y)| x<-ivs', y<-ivs']
    where ivs' = (map (\(x,y) -> Interval x y) (zip a b)) ++ [ Empty ]

-- Interval Tester, given an interval operations
interTest::[(Interval, Interval)]->(Interval -> Interval -> Interval)
            ->[(Interval, Interval, Interval)]
interTest [] _ = []
interTest ((a,b):xs) f = (a, b, c) : interTest xs f
        where c = (f a b)

-- To test the function:
-- interTest ivs intersec2
-- or 
-- interTest ivs intersec

-- intersec2
-- [([-oo,0],[-oo,0],[-oo,0]),
-- ([-oo,0],[-1,3],[-1,0]),
-- ([-oo,0],[0,5],_|_),
-- ([-oo,0],[2,oo],_|_),
-- ([-oo,0],_|_,_|_),
-- ([-1,3],[-oo,0],[-1,0]),
-- ([-1,3],[-1,3],[-1,3]),
-- ([-1,3],[0,5],[0,3]),
-- ([-1,3],[2,oo],[2,3]),
-- ([-1,3],_|_,_|_),
-- ([0,5],[-oo,0],_|_),
-- ([0,5],[-1,3],[-1,5]),
-- ([0,5],[0,5],[0,5]),
-- ([0,5],[2,oo],[2,5]),
-- ([0,5],_|_,_|_),
-- ([2,oo],[-oo,0],_|_),
-- ([2,oo],[-1,3],[2,3]),
-- ([2,oo],[0,5],[2,5]),
-- ([2,oo],[2,oo],[2,oo]),
-- ([2,oo],_|_,_|_),
-- (_|_,[-oo,0],_|_),
-- (_|_,[-1,3],_|_),
-- (_|_,[0,5],_|_),
-- (_|_,[2,oo],_|_),
-- (_|_,_|_,_|_)]


