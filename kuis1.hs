-- 1. KPK
-- KPK Jawaban
kpk a b = head [x | x <- [a..a*b], y <- [b..a*b],   (((x `mod` a) == 0) &&
                                                    ((y `mod` b) == 0)) && 
                                                    (x == y)]

-- KPK Logic
kpk a b * fpb a b = a*b
fpb 0 b = b
fpb a 0 = a
fpb a b = fpb b (a `mod` b)
kpk a b = (a * b) `div` (fpb a b)

-- KPK Using map in list comprehension
kpk a b = head [z | z <- (map (*a) [1..]), z `mod` b == 0]

-- 2. 