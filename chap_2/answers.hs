-- 2.5
myInit :: [a] -> [a]
-- myInit ns = reverse (tail (reverse ns))
myInit ns = take (length ns - 1) ns
