-- 1.4
qsort_dsc [] = []
qsort_dsc (x:xs) = qsort_dsc larger ++ [x] ++ qsort_dsc smaller
    where 
          smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]
