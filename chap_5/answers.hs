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
