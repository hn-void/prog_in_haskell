double x = x + x
quadruple x = double (double x)
-- Factorial of a positive integer:
factorial n = product [1..n]
-- Average of a list of integers:
average ns = sum ns `div` length ns

{- This is a test program -}

n' = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

my_last1 ns = ns !! (length ns - 1)
my_last2 ns = reverse ns !! 0
my_last3 ns = head (reverse ns)
my_last4 ns = head (drop (length ns - 1) ns)

my_init1 ns = reverse (tail (reverse ns))
my_init2 ns = take (length ns - 1) ns
