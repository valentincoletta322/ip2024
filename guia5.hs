quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar t (x:xs) 
        | t /= x = x:(quitar t xs)
        | otherwise = xs

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) 
        | x >= head xs = maximo (x:(tail xs))
        | otherwise = maximo xs


ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar (x:xs)
        | x > head xs =  (ordenar ([head xs]++[x]++tail xs))
        | otherwise = ordenar ([x]++(tail xs)++[head xs])

        -- 532 5>3 -> ordenar 352
        -- 352 3>5 -> 3++
                 --   25

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

-- Implementar una función que me diga si una persona aparece en mi lista de contactos del telefono:
-- enLosContactos (LOS NOMBRES DE PERSONAS SON UNICOS) :: Nombre -> ContactosTel -> Bool


-- revisar el metodo fst
-- si en lugar de c hago una tupla puedo acceder facilmente a todos los datos (si tiene mas)
-- ej si contacto tuviera nmb dni y tel ((nmb, dni, tel):cs) 
enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos  nombre (c:cs)
    | nombre == fst c = True
    | otherwise = enLosContactos nombre cs

-- tambien sirve 
enLosContactosAux :: Nombre -> ContactosTel -> Bool
enLosContactosAux _ [] = False
enLosContactosAux nombre (c:cs) = ( nombre == fst c ) || ( enLosContactos nombre cs )

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto nuevoContacto (c:cs)
    | enLosContactos (fst nuevoContacto) (c:cs) = actualizarContacto nuevoContacto (c:cs)
    | otherwise = nuevoContacto:(c:cs)

actualizarContacto :: Contacto -> ContactosTel -> ContactosTel
actualizarContacto nuevoContacto (c:cs)
    | fst nuevoContacto == fst c = (nuevoContacto:cs)
    | otherwise = c:(actualizarContacto nuevoContacto cs)
