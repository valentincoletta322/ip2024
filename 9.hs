fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci a = fibonacci (a-1) + fibonacci (a-2)

parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = 1 + parteEntera (x-1)

parteEnteraNeg :: Float -> Integer
parteEnteraNeg x | (x < 1) && (x > (-1)) = 0
                 | x <= (-1) = (-1) + parteEnteraNeg (x+1)
                 | x >= 1 = 1 + parteEnteraNeg (x-1)
               
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = div (mod (abs n) (10^(exp))) (10^(exp-1))
    where exp = (cantDigitos n) + 1 - i

cantDigitos :: Integer -> Integer
cantDigitos n | n <= 9 && n >= 0 = 1
              | otherwise = 1 + cantDigitos (div n 10)

esCapicua :: Integer -> Bool
esCapicua n | ultimo /= primero = False
            | ultimo == primero && cantDigitos n <= 2 = True
            | otherwise = esCapicua (sacarBordes n)
            where (ultimo, primero) = ((iesimoDigito n (cantDigitos n)), (iesimoDigito n 1))

sacarBordes :: Integer -> Integer
sacarBordes n = mod (div n 10) (10^((cantDigitos n)-2))

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | n /= 1 = q^(n+m) + sumaPotencias q (n-1) m
                    | m /= 1 = q^(n+m) + sumaPotencias q n (m-1)
                    | otherwise = q^2

                    -- 2+2 1+2 1+1 CORREGIR

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n desde | mod n desde == 0 = desde
                          | otherwise = menorDivisorDesde n (desde+1)

esFibonacci :: Integer -> Bool
esFibonacci n = recorrerFibonacci n 1

recorrerFibonacci n a | fibonacci a > n = False
                      | fibonacci a == n = True
                      | otherwise = recorrerFibonacci n (a+1)


esPrimo :: Integer -> Bool
esPrimo 2 = True
esPrimo 1 = False
esPrimo n = menorDivisor n == n


esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = recorrerPrimos n 2

recorrerPrimos :: Integer -> Integer -> Bool
recorrerPrimos n desde | sumarPrimosHastaN desde 2 > n = False
                       | sumarPrimosHastaN desde 2 == n = True
                       | otherwise = recorrerPrimos n (desde+1)


sumarPrimosHastaN :: Integer -> Integer -> Integer
sumarPrimosHastaN n desde | (desde <= n) && (esPrimo desde) = desde + sumarPrimosHastaN n (desde+1)
                          | (desde <= n) = sumarPrimosHastaN n (desde+1)
                          | otherwise = 0
