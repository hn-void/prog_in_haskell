-- 4.1
halve :: [a] -> ([a], [a])
{- halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs) -}
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

-- 4.2
third :: [a] -> a
{- third xs = head (tail (tail xs)) -}
{- third xs = xs !! 2 -}
third (_:_:x:_) = x

-- 4.3
safetail :: [a] -> [a]
{- safetail xs = if null xs then [] else tail xs -}
{-
safetail xs | null xs = []
            | otherwise = tail xs
-}
safetail [] = []
safetail (_:xs) = xs

-- 4.4
{-
True || True = True
True || False = True
False || True = True
False || False = False

False || False = False
_ || _ = True

False || b = b
True || _ = True

b || c | b == c = b
       | otherwise True
-}

-- 4.5
{-
(&&) :: Bool -> Bool -> Bool
a && b = if a then
              if b then True
                   else False
              else False
-}
{-
*Main> True Main.&& True
True
-}

-- 4.6
(&&) :: Bool -> Bool -> Bool
a && b = if a then b
              else False

-- 4.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- 4.8
luhnDouble :: Int -> Int
luhnDouble x = if double > 9 then double - 9 else double
    where double = x * 2

evenDoubleFromLeft :: (Num a) => [a] -> [a]
evenDoubleFromLeft [] = []
evenDoubleFromLeft (x:xs) = if even (length (x:xs))
                             then x : evenDoubleFromLeft xs
                             else x*2 : evenDoubleFromLeft xs

evenDoubleFromRight :: (Num a) => [a] -> [a]
evenDoubleFromRight = reverse . evenDoubleFromLeft . reverse

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if sum (map luhnDouble (evenDoubleFromRight [a,b,c,d])) `mod` 10 == 0
                  then True else False
