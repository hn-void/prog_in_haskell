import Data.Char

-- 5.1
-- n = sum [x^2 | x <- [1..100]]

-- 5.2
grid :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 5.3
square :: (Num a, Enum a, Eq a) => a -> [(a, a)]
-- square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 5.4
my_replicate :: (Num t, Enum t) => t -> a -> [a]
my_replicate n x = [x | _ <- [1..n]]

-- 5.5
pyths :: (Num t, Eq t, Enum t) => t -> [(t, t, t)]
-- pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       x^2 + y^2 == z^2]

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- 5.7 not confident
-- [(x, y) | x <- [1,2,3], y <- [4,5,6]]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- [[(1,4),(1,5),(1,6)],[(2,4),(2,5),(2,6)],[(3,4),(3,5),(3.6)]]?
l = [x | x <- [1,2,3]] ++ [y | y <- [4,5,6]]

-- 5.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]

-- 5.10
let2int :: Char -> Int
let2int c = ord c - ord 'a'

cap2int :: Char -> Int
cap2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2cap :: Int -> Char
int2cap n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2cap ((cap2int c + n) `mod` 26)
          | otherwise = c
