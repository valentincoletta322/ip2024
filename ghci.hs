doubleMe :: Integer -> Integer
doubleMe x = x+x

doubleMe2 :: Integer -> Integer
doubleMe2 x = doubleMe x *2

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
fog x = f (g x)

gof :: Integer -> Integer
gof x = g (f x)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x>= z = x
              | y>= z = y
              | otherwise = z

maximoV2 :: Integer -> Integer -> Integer -> Integer
maximoV2 x y z = max x (max y z)

maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

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

algunoEsCero1 :: Float -> Float -> Bool
algunoEsCero1 0 _ = True
algunoEsCero1 _ 0 = True
algunoEsCero1 _ _ = False

algunoEsCero2 :: Float -> Float -> Bool
algunoEsCero2 x y = x == 0 || y == 0

ambosSonCero1 :: Float -> Float -> Bool
ambosSonCero1 0 0 = True
ambosSonCero1 _ _ = False

ambosSonCero2 :: Float -> Float -> Bool
ambosSonCero2 x y = x == 0 && y == 0

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod (absoluto x) (absoluto y) == 0

digitoDecenas :: Integer -> Integer
digitoDecenas x = div (mod (absoluto x) 100) 10

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b | (a*a) + a*b * calcularK a b == 0 = True
                      | otherwise = False

calcularK :: Integer -> Integer -> Integer
calcularK a b = div (a*a) (-(a*b))

prodInt :: (Float, Float) -> (Float, Float) -> (Float, Float)
prodInt (x1, y1) (x2, y2) = (x1*y1, x2*y2)

todoMenor2 :: (Float, Float) -> (Float, Float) -> Bool
todoMenor2 (x1, y1) (x2, y2) = x1<x2 && y1<y2

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) =  sqrt ((x1-x2)**2 + (y1-y2)**2)

-- esto no da el resultado esperado -> sqrt ((x1-x2)**2)+((y1-y2)**2) ???
-- es por un tema de espacios -> Primero hace sqrt(x1-x2**2) y despues le suma la otra parte ???

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y, z) = x+y+z

sumaSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumaSoloMultiplos (x, y ,z) k = (x*a)+(y*b)+(z*c)
    where (a,b,c) = (saberSiEsMultiplo x k, saberSiEsMultiplo y k, saberSiEsMultiplo z k)
-- puedo hacer algo del estilo x*(mod (absoluto x) k == 0) ?? requiere cast?
-- seria mejor usar guardas?

saberSiEsMultiplo :: Integer -> Integer -> Integer
saberSiEsMultiplo num k | mod (absoluto num) k == 0 = 1
                        | otherwise = 0
-- especie de map

posPrimerPar ::  (Integer, Integer, Integer) -> Integer
posPrimerPar (x, y, z) | saberSiEsMultiplo x 2 == 1 = 1
                       | saberSiEsMultiplo y 2 == 1 = 2
                       | saberSiEsMultiplo z 2 == 1 = 3
                       | otherwise = 4

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)
-- infiere el tipo, es lo que se espera?

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

-- igual que el anterior

type Punto2D = (Float, Float)

prodInt2 :: Punto2D -> Punto2D -> Punto2D
prodInt2 (x1, y1) (x2, y2) = (x1*y1, x2*y2) -- Declaro los parametros igual?

ft :: Integer -> Integer
ft n | n <= 7 = n*n
     | n > 7 = 2*n - 1

gt :: Integer -> Integer
gt n | saberSiEsMultiplo n 2 == 1 = div n 2 - 1
     | otherwise = 3*n + 1

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x, y, z) = (ft x) > (gt x) && (ft y) > (gt y) && (ft z) > (gt z)

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto anio | saberSiEsMultiplo anio 4 == 0 = False
              | (saberSiEsMultiplo anio 400 == 0) && (saberSiEsMultiplo anio 100 == 1) = False
              | otherwise = True

