-- Bounds node IO operations of the BTree
--
-- Brain Computations:
-- Let d(n) = decr n
-- Then d(n) = 3 + 2*(3 + 2*(3 + 2*d(n-3)))
--           = 3 * \sum_{i=0}^{n-2} 2^i  +  2^{n-1}
--           = 3 * (2^{n-1} - 1)  +  2^{n-1}
--           = 4 * 2^{n-1}  -  3
--           = 2^{n+1} - 3
-- Let i(n) = incr n and assume n >= 2
-- Then i(n) = 2 + i(n-1) + s(n-1)
--           = 2 + (2 + i(n-2) + s(n-2)) + s(n-1)
--           = 2 + (2 + i(n-2) + d(n-2)) + d(n-1)
--           = 3 + 2 * (n-1)  +  \sum_{i=1}^{n-1} d(i)  +  1
--           = 3 + 2 * (n-1)  +  \sum_{i=1}^{n-1} (2^{i+1} - 3) - 1
--           = 3 + 2 * (n-1)  +  \sum_{i=1}^{n-1} 2^{i+1}  -  3 * (n-1) - 1
--           = 3 - (n-1)  +  \sum_{i=1}^{n-1} 2^{i+1}  -  1
--           = 3 - (n-1)  +  \sum_{i=0}^{n} 2^{i}  -  (2^0 + 2^1) - 1
--           = - n + 1  +  \sum_{i=0}^{n} 2^{i}  -  1
--           = 2^{n+1} - n - 1

insertion :: Integer -> Integer
insertion n = n + incr n

deletion :: Integer -> Integer
deletion  n = n + decr n

incr :: Integer -> Integer
incr 1 = 3
incr n = 2 + incr (n-1) + sync (n-1)

decr :: Integer -> Integer
decr 1 = 1
decr n = 3 + 2 * sync (n-1)

sync :: Integer -> Integer
sync 1 = 0
sync n = maxl [ 1 + sync (n-1), decr n, incr n ]

maxl :: Ord a => [a] -> a
maxl (x : xs) = maxh x xs 
        where   maxh i []                   = i
                maxh i (x : xs) | i < x     = maxh x xs
                maxh i (x : xs) | otherwise = maxh i xs

mklist :: (Integer -> Integer) -> Integer -> Integer -> [ Integer ]
mklist _ _ 0 = []
mklist _ m n | m > n = []
mklist f m n = (f m) : mklist f (m+1) n

decrs :: Integer -> [ Integer ]
decrs n = mklist decr 1 n

incrs :: Integer -> [ Integer ]
incrs n = mklist incr 1 n

syncs :: Integer -> [ Integer ]
syncs n = mklist sync 1 n

pots :: Integer -> [ Integer ]
pots n = mklist (power 2) 0 n

diffs :: Integer -> [ Integer ]
diffs n = map (\(x,y) -> x - y) (zip (ds n) (ps n))
        where   ds :: Integer -> [ Integer ]
                ds n = decrs n

                ps :: Integer -> [ Integer ]
                ps n = map (\x -> 2*x) (skipFirst (pots n))
                
                skipFirst :: [a] -> [a]
                skipFirst (x : xs) = xs
 
power :: Integer -> Integer -> Integer
power x 0 = 1
power x y | y > 0 && y `mod` 2 == 0 = power_X_Ydiv2 * power_X_Ydiv2 
          | y > 0                   = x * x `power` (y-1)
        where power_X_Ydiv2 = x `power` (y `div` 2)

sumf i m f = sum (mklist f i m)

test n = 3 + 2*(n-1) + (sum (decrs (n-1))) + 1

test1 n = 2 + (2 + incr (n-2) + sync (n-2)) + sync (n-1)
test2 n = 2 + (2 + incr (n-2) + decr (n-2)) + decr (n-1)
test3 n = 3 + 2 * (n-1)  +  (sumf (1) (n-1) (\i -> decr(i)))  +  1
test4 n = 3 + 2 * (n-1)  +  (sumf (1) (n-1) (\i -> 2 `power` (i+1) - 3)) - 1
test5 n = 3 + 2 * (n-1)  +  (sumf (1) (n-1) (\i -> 2 `power` (i+1)))  -  3 * (n-1) - 1
test6 n = 3 - (n-1)  +  (sumf 1 (n-1) (\i -> 2 `power` (i+1)))  -  1
test7 n = 3 - (n-1)  +  (sumf 0 n (\i -> 2 `power` i))  -  (2 `power` 0 + 2 `power` 1) - 1
test8 n = - n + 1  +    (sumf 0 n (\i -> 2 `power` i))  -  1
test9 n = - n - 1  +  2 `power` (n+1)


