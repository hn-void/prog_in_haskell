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
