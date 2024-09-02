-- pwd

doubleMe :: Integer -> Integer
doubleMe x = x+x

doubleMe2 :: Integer -> Integer
doubleMe2 x = doubleMe(x)*2

f1 :: Integer -> Integer
f1 x | x == 1 = 8 
    | x == 4 = 131
    | x == 16 = 16
    | otherwise = 0

f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16
f _ = 0

g :: Integer -> Integer
g 8 = 16
g 131 = 1
g 16 = 4
g _ = 0

fog :: Integer -> Integer
fog x = f(g(x))

gof :: Integer -> Integer
gof x = g(f(x)) 

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x>= z = x
              | y>= z = y
              | otherwise = z

maximoV2 :: Integer -> Integer -> Integer -> Integer
maximoV2 x y z = max x (max y z)

absoluto ::  Integer -> Integer
absoluto x | x *(-1) > x = x*(-1)
           | otherwise = x

sumaSoloDistintos :: Integer -> Integer -> Integer -> Integer
sumaSoloDistintos x y z | x == y && y == z = 0
                        | x == y = z
                        | x == z = y
                        | y == z = x
                        | otherwise = x+y+z

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod (absoluto x) 10

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1, y1) (x2, y2) = not ((x1 > x2) || (y1 > y2))
