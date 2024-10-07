type Nombre = String
type Notas = [Int]
type Alumno = (Nombre, Notas)

funcionLength :: [t] -> Int
funcionLength [] = 0
funcionLength (t:ts) = 1 + funcionLength ts

funcionElem :: (Eq t) => t -> [t] -> Bool
funcionElem _ [] = False
funcionElem a (t:ts) = a == t || funcionElem a ts

aproboMasDeNMaterias :: [Alumno] -> Nombre -> Int -> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias (a:as) alumno n
    | fst a == alumno = materiasAprobadas (snd a) > n
    | otherwise = aproboMasDeNMaterias as alumno n

materiasAprobadas :: [Int] -> Int
materiasAprobadas [] = 0
materiasAprobadas (n:ns)
    | n >= 4 = 1 + materiasAprobadas ns
    | otherwise = materiasAprobadas ns


alumnos :: [Alumno]
alumnos = [("Jorge", [10,10,10,10]), ("Maria", [8,8,9,8]), ("Esteban", [10,10,10,4])]

buenosAlumnos :: [Alumno] -> [Nombre]
buenosAlumnos [] = []
buenosAlumnos (a:as)
    | noTuvoAplazos a && (calcularPromedio (snd a) >= 8) = (fst a):(buenosAlumnos as)
    | otherwise = (buenosAlumnos as)

calcularPromedio :: Notas -> Float
calcularPromedio [] = 0
calcularPromedio notas =  fromIntegral (sumatoriaNotas notas) / fromIntegral (funcionLength notas)

sumatoriaNotas :: Notas -> Int
sumatoriaNotas [] = 0
sumatoriaNotas (n:ns) = n + sumatoriaNotas ns

noTuvoAplazos :: Alumno -> Bool
noTuvoAplazos (nombre, []) = True
noTuvoAplazos (nombre, (n:ns))
    | n > 3 = noTuvoAplazos (nombre, ns)
    | otherwise = False

mejorPromedio :: [Alumno] -> Nombre
mejorPromedio [a] = fst a
mejorPromedio (a:as)
    | calcularPromedio (snd a) >= calcularPromedio (snd (head as)) = mejorPromedio (a:tail as)
    | otherwise = mejorPromedio (as)

seGraduoConHonores :: [Alumno] -> Int -> Nombre -> Bool
seGraduoConHonores (a:as) cant nombre = (aproboMasDeNMaterias (a:as) nombre (cant-1)) && (funcionElem nombre (buenosAlumnos (a:as))) && (calcularPromedio (notas (a:as) (mejorPromedio (a:as))) - (calcularPromedio (notas (a:as) nombre)) < 1)

notas :: [Alumno] -> Nombre -> [Int]
notas (a:as) nombre
    | fst a == nombre = snd a
    | otherwise = notas as nombre