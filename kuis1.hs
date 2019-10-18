-- KPK Jawaban
kpk a b = head [x | x <- [a..a*b], y <- [b..a*b],   x `mod` a == 0 and
                                                    y `mod` b == 0 and 
                                                    x == y]
