-- 4.8.1
halve :: [a] -> ([a], [a])
{- halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs) -}
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

-- 4.8.2
third :: [a] -> a
{- third xs = head (tail (tail xs)) -}
{- third xs = xs !! 2 -}
third (_:_:x:_) = x

-- 4.8.3
safetail :: [a] -> [a]
{- safetail xs = if null xs then [] else tail xs -}
{-
safetail xs | null xs = []
            | otherwise = tail xs
-}
safetail [] = []
safetail (_:xs) = xs

-- 4.8.4
{-
True || True = True
True || False = True
False || True = True
False || False = False
-}
{-
False || False = False
_ || _ = True
-}
{-
False || b = b
True || _ = True
-}
{-
b || c | b == c = b
       | otherwise True
-}
