--------------- Problema relacionesValidas ---------------

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [x] = esValido x
relacionesValidas (r:rs)
    | esValido r && (noEstaRepetido r rs) = relacionesValidas rs
    | otherwise = False

esValido :: (String, String) -> Bool
esValido (comp1, comp2) = not (comp1 == comp2)

noEstaRepetido :: (String, String) -> [(String, String)] -> Bool
noEstaRepetido _ [] = True
noEstaRepetido rel (r:rs)
    | rel == r || (fst rel == snd r) && (snd rel == fst r) = False
    | otherwise = noEstaRepetido rel rs

--------------- Problema personas ---------------

personas :: [(String, String)] -> [String]
personas [] = []
personas ((comp1, comp2):rs) = crearListaDePersonas ((comp1, comp2):rs) []

crearListaDePersonas :: [(String, String)] -> [String] -> [String]
crearListaDePersonas [] lista = lista
crearListaDePersonas ((comp1, comp2):rs) lista
    | not (funcionElem comp1 lista) = crearListaDePersonas ((comp1, comp2):rs) (comp1:lista)
    | not (funcionElem comp2 lista) = crearListaDePersonas rs (comp2:lista)
    | otherwise = crearListaDePersonas rs lista

funcionElem :: String -> [String] -> Bool
funcionElem _ [] = False
funcionElem elem (x:xs)= (elem == x) || funcionElem elem xs

--------------- Problema amigosDe ---------------

amigosDe :: String -> [(String, String)] -> [String]
amigosDe persona relaciones = crearListaDeAmigos persona relaciones []

crearListaDeAmigos :: String -> [(String, String)] -> [String] -> [String]
crearListaDeAmigos _ [] lista = lista
crearListaDeAmigos persona ((comp1, comp2):rs) lista
    | persona == comp1 = crearListaDeAmigos persona rs (comp2:lista)
    | persona == comp2 = crearListaDeAmigos persona rs (comp1:lista)
    | otherwise = crearListaDeAmigos persona rs lista

--------------- Problema personaConMasAmigos ---------------

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos relaciones = obtenerMaximo (amigosPorPersona relaciones 0 [])

obtenerMaximo :: [(String, Int)] -> String
obtenerMaximo [(x, y)] = x
obtenerMaximo ((persona, amigos):xs)
    | amigos >= snd (head xs) = obtenerMaximo ((persona, amigos):(tail xs))
    | otherwise = obtenerMaximo xs

amigosPorPersona :: [(String, String)] -> Int -> [(String, Int)] -> [(String, Int)]
amigosPorPersona ((comp1, comp2):rs) contados listaPersonas
    | contados < (funcionLen ((comp1, comp2):rs)) = amigosPorPersona (rs++[(comp1,comp2)]) (contados+1) ((crearPar (comp1, comp2) relaciones)++listaPersonas)
    | otherwise = listaPersonas
    where relaciones = ((comp1, comp2):rs)

cantidadDeAmigos :: String -> [(String, String)] -> Int
cantidadDeAmigos persona relaciones = funcionLenAlt (amigosDe persona relaciones)

crearPar :: (String, String) -> [(String, String)] -> [(String, Int)]
crearPar (comp1, comp2) relacion = [(comp1, (cantidadDeAmigos comp1 relacion)), (comp2, (cantidadDeAmigos comp2 relacion))]

funcionLen :: [(String, String)] -> Int
funcionLen [] = 0
funcionLen (x:xs) = 1 + funcionLen xs

funcionLenAlt :: [String] -> Int
funcionLenAlt [] = 0
funcionLenAlt (x:xs) = 1 + funcionLenAlt xs
