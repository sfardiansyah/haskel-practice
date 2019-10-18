-- 1. KPK
-- KPK Jawaban
kpk a b = head [x | x <- [a..a*b], y <- [b..a*b],   (((x `mod` a) == 0) &&
                                                    ((y `mod` b) == 0)) && 
                                                    (x == y)]


-- KPK Logic
-- kpk a b * fpb a b = a*b
fpb 0 b = b
fpb a 0 = a
fpb a b = fpb b (a `mod` b)
-- kpk a b = (a * b) `div` (fpb a b)


-- KPK Using map in list comprehension
-- kpk a b = head [z | z <- (map (*a) [1..]), z `mod` b == 0]


-- 2. (x, y) pair
xypair = [(x,y) | x <- [1..3], y <- [1..(2*x)]]

-- (x, y) pair with map
pair a b = (a, b)
getPair [] = []
getPair (x:xs) = map (pair x) [1..2*x] ++ getPair xs
-- xypair = getPair [1..3]

-- 3. Merge Sort
merge [] ys = ys
merge xs [] = xs
merge kiri@(x:xs) kanan@(y:ys)      | x < y     = x : merge xs kanan
                                    | x >= y    = y : merge kiri ys
mergeSort xs = merge (mergeSort (take (length xs) xs)) (mergeSort (drop (length xs) xs))