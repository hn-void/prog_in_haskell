-- 7.1
-- map f (filter p xs)

-- 7.2
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = and . map p

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = or . map p

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []                 = []
myTakeWhile p (x:xs) | p x       = x : myTakeWhile p xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x:xs) | p x       = myDropWhile p xs
                     | otherwise = x:xs

-- 7.3
myMap :: (a -> a) -> [a] -> [a]
-- myMap f = foldr ((:).f) []
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x : xs else xs) []

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 7.5
myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x y -> f (x,y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x,y) -> f x y
