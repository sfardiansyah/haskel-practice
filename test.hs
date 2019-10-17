-- length ls = sum (map (+1) ls)

-- halo xs = map (+1) (map (+1) xs)

-- iter 0 f x = x
-- iter n f x = f (iter (n-1) f x)

-- add :: a -> a -> a
add a 0 = a
add a b = add (succ a) (pred b)