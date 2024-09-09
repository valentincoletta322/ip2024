fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci a = fibonacci (a-1) + fibonacci (a-2)

parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = 1 + parteEntera (x-1)

parteEnteraN :: Float -> Integer
parteEnteraN x | (x < 1) && (x > (-1)) = 0
               | x <= (-1) = 1 + parteEnteraN (x+1)
               | x >= 1 = 1 + parteEnteraN (x-1)
               
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = div (mod (abs n) (10^(exp))) (10^(exp-1))
    where exp = (cantDigitos n) + 1 - i

cantDigitos :: Integer -> Integer
cantDigitos n | n <= 9 && n >= 0 = 1
              | otherwise = 1 + cantDigitos (div n 10)

esCapicua :: Integer -> Bool
esCapicua n | ultimo /= primero = False
            | ultimo == primero && cantDigitos n <= 2 = True
            | otherwise = esCapicua (mod (div n 10) (10^((cantDigitos n)-2)))
            where (ultimo, primero) = ((iesimoDigito n (cantDigitos n)), (iesimoDigito n 1))