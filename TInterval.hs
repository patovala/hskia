-- A super interval calculator
--
-- PV


module TInterval (Interval(..),Lb(..),Ub(..), union, intersec)
where
import Data.List(sort)

-- Interval ej: [-1, 1] or [-oo, oo] or [1,oo]
data Interval 
   = Empty 
   | Interval Lb Ub

-- Lower bound: -1,1,-oo
data Lb =  MinInf | Lb {valueLb :: Int} 
    deriving (Ord)
 -- deriving (Show,Eq)

-- Upper bound: -1,1,-oo
data Ub = Ub {valueUb :: Int} | PlusInf
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
    (+) x PlusInf = PlusInf 
    (+) PlusInf x = PlusInf 
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

-- PV this define the intersection of two intervals
intersec2 :: Interval -> Interval -> Interval
intersec2 Empty x = Empty 
intersec2 x Empty = Empty 
intersec2 (Interval (Lb a) (Ub b)) (Interval (Lb c) (Ub d)) 
    | b > c && a<d = Interval (Lb c) (Ub b)
intersec2 (Interval MinInf PlusInf) i = i 
intersec2 i (Interval MinInf PlusInf) = i 
-- new rules for infinites
-- both lb with -oo
intersec2 (Interval lb1 ub1) (Interval lb2 ub2)
    | lb1 == MinInf && lb2 == MinInf = Interval MinInf ub
                where ub = if (ub1 >= ub2) then ub2 else ub1
-- both ub with oo
intersec2 (Interval lb1 ub1) (Interval lb2 ub2)
    | ub1 == PlusInf && ub2 == PlusInf = Interval lb PlusInf
                where lb = if (lb1 >= lb2) then lb2 else lb1
-- lb1 == -oo and ub2 == oo ub1 can't be oo since it was previously catched
intersec2 (Interval lb1 ub1) (Interval lb2 ub2)
    | lb1 == MinInf && ub2 == PlusInf = r
                where (Lb x) = lb2
                      (Ub y) = ub1
                      r = case (compare x y) of
                                EQ -> Empty
                                GT -> Empty 
                                LT -> (Interval (Lb y) (Ub x))

-- ub1 == oo and lb2 == -oo ub2 can't be oo since it was previously catched
intersec2 (Interval lb1 ub1) (Interval lb2 ub2)
    | lb2 == MinInf && ub1 == PlusInf = r
                where (Lb x) = lb1
                      (Ub y) = ub2
                      r = case (compare x y) of
                                EQ -> Empty
                                GT -> Empty
                                LT -> Interval (Lb x) (Ub y)
-- [-oo,a] [b,c]
intersec2 (Interval MinInf ub1) (Interval lb2 ub2) = r
                where
                    (Ub x) = ub1
                    (Lb y) = lb2
                    (Ub z) = ub2
                    r = if (x<=y && x<=z) then Empty else (Interval (Lb x') (Ub y'))
                    (x':y':z':[]) = Data.List.sort (x:y:z:[])
-- [a,oo] [b,c]
intersec2 (Interval lb1 PlusInf) (Interval lb2 ub2) = r
                where
                    (Lb x) = lb1
                    (Lb y) = lb2
                    (Ub z) = ub2
                    r = if (x>=y && x>=z) then Empty else (Interval (Lb y') (Ub z'))
                    (x':y':z':[]) = Data.List.sort (x:y:z:[])
-- [a,b] [-oo,c]
intersec2 (Interval lb1 ub1) (Interval MinInf ub2) = r
                where
                    (Lb x) = lb1
                    (Ub y) = ub1
                    (Ub z) = ub2
                    r = if (z<=x && z<=y) then Empty else (Interval (Lb x') (Ub y'))
                    (x':y':z':[]) = Data.List.sort (x:y:z:[])
-- [a,b] [c,oo]
intersec2 (Interval lb1 ub1) (Interval lb2 PlusInf) = r
                where
                    (Lb x) = lb1
                    (Ub y) = ub1
                    (Lb z) = lb2
                    r = if (z>=x && z>=y) then Empty else (Interval (Lb y') (Ub z'))
                    (x':y':z':[]) = Data.List.sort (x:y:z:[])
--
--intersec2 _ _ = Empty

intersec :: Interval -> Interval -> Interval
intersec (Interval (Lb lb1)(Ub ub1)) (Interval (Lb lb2) (Ub ub2))
   = Interval (Lb (maximum (lb1:lb2:[]))) (Ub (minimum (ub1:ub2:[]))) 
intersec (Interval MinInf PlusInf) (Interval (Lb lb2) (Ub ub2))
   = (Interval (Lb lb2) (Ub ub2))
intersec (Interval (Lb lb1)(Ub ub1))(Interval MinInf PlusInf) 
   = (Interval (Lb lb1)(Ub ub1))
intersec (Interval (Lb lb1) PlusInf)(Interval (Lb lb2) PlusInf) 
   |lb1 >= lb2 = Interval (Lb lb1) PlusInf
   |otherwise = Interval(Lb lb2) PlusInf
intersec (Interval (Lb lb1)(Ub ub1))(Interval (Lb lb2) PlusInf) 
   |lb1 >= lb2 = Interval (Lb lb1)(Ub ub1)
   |ub1 < lb2 = Empty
   |otherwise = Interval(Lb ub1)(Ub lb2)
intersec (Interval (Lb lb1) PlusInf) (Interval (Lb lb2)(Ub ub2))
   |lb2 >= lb1 = (Interval (Lb lb2)(Ub ub2))
   |ub2 > lb1 = Empty
   |otherwise = Interval(Lb lb1)(Ub ub2)
intersec (Interval (Lb lb1)(Ub ub1))(Interval MinInf (Ub ub2)) 
   |ub1 > ub2 && lb1<ub2 = Interval(Lb lb1)(Ub ub2)
   |ub1 > ub2 && lb1>ub2 = Empty
   |otherwise = Interval(Lb lb1)(Ub ub1)
intersec (Interval MinInf (Ub ub2))(Interval (Lb lb1)(Ub ub1)) 
   |ub1 > ub2 && lb1<ub2 = Interval(Lb lb1)(Ub ub2)
   |ub1 > ub2 && lb1>ub2 = Empty
   |otherwise = Interval(Lb lb1)(Ub ub1)
intersec (Interval MinInf (Ub ub1))(Interval(Lb lb2) PlusInf) 
   |lb2 > ub1  = Interval(Lb lb2)(Ub ub1)
   |ub1 < lb2  = Empty
   |otherwise = Interval(Lb lb2)(Ub ub1)
intersec (Interval(Lb lb2) PlusInf)(Interval MinInf (Ub ub1)) 
   |lb2 > ub1  = Interval(Lb lb2)(Ub ub1)
   |ub1 < lb2  = Empty
   |otherwise = Interval(Lb lb2)(Ub ub1)
intersec (Interval MinInf (Ub ub1) )(Interval MinInf (Ub ub2)) 
   |ub2 < ub1  = (Interval MinInf (Ub ub2))
   |otherwise = (Interval MinInf (Ub ub1) )
{--intersec (Interval (Lb lb1) PlusInf)(Interval (Lb lb2) PlusInf) 
   |lb2 > lb1  = (Interval (Lb lb2) PlusInf)
   |otherwise = (Interval (Lb lb1) PlusInf)--}

   
{--biggerLess::Interval->Interval->Interval
biggerLess(Interval (Lb lb1)(Ub ub1))(Interval (Lb lb2)(Ub ub2))
   = Order((lb1>lb2) || (ub1>ub2))((lb1<lb2) || (ub1<ub2) || (ub1==ub2))
biggerLess(Interval MinInf(Ub ub1))(Interval (Lb lb2)(Ub ub2))
   = Order((ub1>ub2))(True)
biggerLess(Interval (Lb lb1)PlusInf)(Interval (Lb lb2)(Ub ub2))
   = Order(True)(lb1<lb2)
biggerLess(Interval MinInf PlusInf) _
   = Order(True)(True)   
  
   
sameDifferent::Interval->Interval->Interval
sameDifferent(Interval (Lb lb1)(Ub ub1))(Interval (Lb lb2)(Ub ub2))
   = Order((lb1==lb2) && (ub2==ub1))((lb1/=lb2) || (ub2/=ub1)) --}

-- Test the interval behaviour
a = [(MinInf),(Lb (-1)),(Lb 0),(Lb 2),(Lb 4)]
b = [(Ub 0),(Ub 3),(Ub 5),PlusInf]

ivs = [(x,y)| x<-ivs', y<-ivs']
    where ivs' = (map (\(x,y) -> Interval x y) (zip a b)) ++ [ Empty ]

-- Interval Tester, given an interval operations
interTest::[(Interval, Interval)]->(Interval -> Interval -> Interval)->[(Interval, Interval, Interval)]
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


