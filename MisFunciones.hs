module MisFunciones where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

pares :: [Integer] -> [Integer]
pares xs = paresSinRepetir (paresRepetidos xs)

paresRepetidos :: [Integer] -> [Integer]
paresRepetidos [] = []
paresRepetidos (x:xs)
     | mod (abs x) 2 == 0 = x:paresRepetidos xs
     | otherwise = paresRepetidos xs

paresSinRepetir :: [Integer] -> [Integer]
paresSinRepetir [] = []
paresSinRepetir [x] = [x]
paresSinRepetir (x:xs)
    | elem x xs = paresSinRepetir xs
    | otherwise = x:paresSinRepetir xs

muchosMaximos :: [Integer] -> [Integer]
muchosMaximos lista = obtenerIndices lista (obtenerMaximo lista)

obtenerMaximo :: [Integer] -> Integer
obtenerMaximo [x] = x
obtenerMaximo (x:xs)
    | x > head xs = obtenerMaximo (x:tail xs)
    | otherwise = obtenerMaximo xs

indiceDe :: Integer -> [Integer] -> Integer
indiceDe n (x:xs)
    | n == x = 0
    | otherwise = 1 + (indiceDe n xs)

obtenerIndices :: [Integer] -> Integer -> [Integer]
obtenerIndices [] _ = []
obtenerIndices lista n
    | elem n lista = indiceDe n lista:obtenerIndices (quitar lista n) n
    | otherwise = []

quitar :: [Integer] -> Integer -> [Integer]
qutiar [] _ = []
quitar (x:xs) n 
    | x == n = (x+1):xs
    | otherwise = x:(quitar xs n)


-- para comparar flotantes aproximo -> aproximar f1 f2 = f1 - f2 < e (donde e = 0,0001)

-- Ejericio de repaso

generarStock :: [String] -> [(String, Integer)]
generarStock [] = []
generarStock (n:ns) = agregarProducto n (generarStock ns)

agregarProducto :: String -> [(String, Integer)] -> [(String, Integer)]
agregarProducto x [] = [(x, 1)]
agregarProducto nombre (p:ps)
    | nombre == fst p =  (fst p, (snd p)+1):ps
    | otherwise = p:agregarProducto nombre ps

-- si en lugar de sumar de a 1 borro todos los repetidos hago menos llamados recursivos
