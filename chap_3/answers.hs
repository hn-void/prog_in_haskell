-- 3.3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: (Num a) => a -> a
double x = x*2

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a 
twice f x = f (f x)

-- 3.4
{-
Because a function is not necessarily a global function.
Two functions of the same type are equivalent when they return equivalent results for equivalent arguments.
-}
