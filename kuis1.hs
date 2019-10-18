-- 1. KPK
-- KPK Jawaban
-- kpk a b = head [x | x <- [a..a*b], y <- [b..a*b],   (((x `mod` a) == 0) &&
--                                                     ((y `mod` b) == 0)) && 
--                                                     (x == y)]

-- KPK Logic
-- kpk a b * fpb a b = a*b
fpb :: Int -> Int -> Int
fpb 0 b = b
fpb a 0 = a
fpb a b = fpb b (a `mod` b)
kpk :: Int -> Int -> Int
kpk a b = (a * b) / (fpb a b)