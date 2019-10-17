import Data.List

divisor n = [x | x <- [1 .. n], n `mod` x == 0]

perm [] = [[]]
perm ls = [x:ps | x <- ls, ps <- perm(ls\\[x])]

pythaTriple = [(x,y,z) | z <- [5 ..], y <- [z, z-1 .. 1], x <- [y, y-1 .. 1], x*x + y*y == z*z]

add [] [] = []
add (a:as) (b:bs) = (a+b) : (add as bs)
fibs = 1 : 1 : add fibs (tail fibs)