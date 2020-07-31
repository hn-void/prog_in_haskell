-- 6.1
fac :: (Num a, Eq a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

my_fac :: (Ord a, Num a) => a -> a
my_fac 0 = 1
my_fac n | n > 0 = n * my_fac (n-1)

{-
<interactive>:34:1: error:
    • Non type-variable argument in the constraint: Num (a -> a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Ord a, Num a, Num (a -> a)) => a -> a
-}

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 6.3
{-
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n-1))
-}

-- 6.4
{-
euclid :: Int -> Int -> Int
euclid m n | ma `mod` mi == 0 = mi
           | otherwise = euclid mi rem
            where mi = min m n
                  ma = max m n
                  rem = ma `mod` mi
-}

{-
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | otherwise = euclid (ma - mi) mi
    where ma = max m n
          mi = min m n
-}

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x-y) y
           | y > x = euclid x (y-x)

-- 6.5
{-
length [1,2,3]
= 1 + length [2,3]
= 1 + 1 + length [3]
= 1 + 1 + 1 + length []
= 1 + 1 + 1 + 1
= 4

drop 3 [1,2,3,4,5]
= drop 2 [2,3,4,5]
= drop 1 [3,4,5]
= drop 0 [4,5]
= [4,5]

init [1,2,3]
= 1 : init [2,3]
= 1 : 2 : init [3]
= 1 : 2 : []
= [1,2] 
-}

-- 6.6
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

myDoubleExclamation :: [a] -> Int -> a
myDoubleExclamation (x:xs) 0 = x
myDoubleExclamation (x:xs) n = myDoubleExclamation xs (n-1)
{-
*Main> [1,2,3,4,5,6,7,8,9] `myDoubleExclamation` 3
4
-}

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

-- 6.7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y
                         then x : merge xs (y:ys)
                         else y : merge (x:xs) ys

-- 6.8
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

-- msort :: Ord a => a -> [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
    where (l,r) = halve xs

-- 6.9
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 xs = []
myTake n (x:xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
