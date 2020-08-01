import Data.Char

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

-- 7.6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

myMap2 :: (a -> a) -> [a] -> [a]
myMap2 f = unfold null (f . head) tail

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\_ -> False) f f

-- 7.7
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

{-
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)
-}

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

addParity :: [Bit] -> [Bit]
addParity bits = (if odd (count 1 bits) then 1 else 0) : bits

checkParity :: [Bit] -> [Bit]
checkParity bits = if even (count 1 bits) then (tail bits) else error "Parity Error"

normalTransmit :: String -> String
normalTransmit = decode . addParity . channel . checkParity . encode

errorChannel :: [Bit] -> [Bit]
errorChannel = tail

errorTransmit :: String -> String
errorTransmit = decode . addParity . errorChannel . checkParity . encode

{-
If the error channel drops "1", then the count of "1" changes and is detected as an error. 
However, if the channel drops "0", the count of "1" doesn't change so that it cannot be detected.

*Main> errorTransmit "higher-functions are easy"
"*** Exception: Parity Error
CallStack (from HasCallStack):
  error, called at answers.hs:91:65 in main:Main

*Main> errorTransmit "higher-functions are difficult"
"\180\180\&3\180\&2\185\SYN\179:\183\&1\186\180\&7\183\&9\144\&0\185\&2\DLE\178\&43\179\180\177:6:"
-}
