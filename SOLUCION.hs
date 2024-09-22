type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

-- SUPONGO QUE NO PERO TENGO QUE RESPETAR LOS NOMBRES DE LOS PARAMETROS???????
------------------ EJERCICIO 1 ------------------

vueloValido :: Vuelo -> Bool
vueloValido (origen, destino, duracion) = (origen /= destino) && (duracion > 0) -- Si el origen es distinto al destino y su duracion es mayor a 0 devuelve True

-- las primeras componentes no pueden ser iguales y la duracion distinta?
-- Tengo que parsear el texto? -> Supongo que no porque no pasarian los tests

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (vuelo:vuelos)
    | vueloValido vuelo && not (tieneVueloRepetido (vuelo:vuelos)) = vuelosValidos vuelos -- Se fija que el vuelo sea valido y que no este repetido
    | otherwise = False
    -- Esta funcion llama a "tieneVueloRepetido" para verificar que "vuelo" no este en la lista "vuelos"

tieneVueloRepetido :: AgenciaDeViajes -> Bool -- Se fija si el primer "vuelo" esta repetido en la lista
tieneVueloRepetido [x] = False
tieneVueloRepetido (vuelo:vuelos)
    | not (dosVuelosIguales vuelo (head vuelos)) = tieneVueloRepetido (vuelo:tail vuelos) -- Si no es igual al head, pasa al siguiente
    | otherwise = True -- En caso contrario, el vuelo esta repetido.
    -- Esta funcion llama a "dosVuelosIguales" para comparar un vuelo con todos los otros de la lista.

dosVuelosIguales :: Vuelo -> Vuelo -> Bool
dosVuelosIguales (origen1, destino1, _) (origen2, destino2, _) = (origen1 == origen2) && (destino1 == destino2)
    -- Devuelve true si los origenes y los destinos son iguales entre si

------------------ EJERCICIO 2 ------------------

ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad] -- Otra vez, me tengo que fijar que sea valido??? Otra, el ida y vuelta como cuenta?
ciudadesConectadas [] _ = []
ciudadesConectadas ((origen, destino, _):vuelos) ciudad
    | ciudad == origen = destino:ciudadesConectadas vuelos ciudad -- Si es igual al origen, agrego el destino como conexion
    | ciudad == destino = origen:ciudadesConectadas vuelos ciudad -- Si es igual al destino, agrego el origen como conexion
    | otherwise = ciudadesConectadas vuelos ciudad -- Ignoro el vuelo actual y sigo con la lista

-- Podria usar las funciones destinosDeUnaCiudad y vuelosHaciaUnaCiudad para reutilizar codigo

destinosDeUnaCiudad :: AgenciaDeViajes -> Ciudad -> [Ciudad] -- Si quiero obtener solo los destinos (para facilitar la informacion en otras funciones)
destinosDeUnaCiudad [] _ = []
destinosDeUnaCiudad ((origen, destino, duracion):vuelos) ciudad
    | ciudad == origen = destino:destinosDeUnaCiudad vuelos ciudad -- Si coincide con el origen, agrego el destino
    | otherwise = destinosDeUnaCiudad vuelos ciudad

vuelosHaciaCiudad :: AgenciaDeViajes -> Ciudad -> [Ciudad] -- Si quiero obtener solo los origenes (para facilitar la informacion en otras funciones)
vuelosHaciaCiudad [] _ = []
vuelosHaciaCiudad ((origen, destino, duracion):vuelos) ciudad
    | ciudad == destino = origen:vuelosHaciaCiudad vuelos ciudad -- Si coincide con el destino, agrego el origen
    | otherwise = vuelosHaciaCiudad vuelos ciudad

------------------ EJERCICIO 3 ------------------
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes -- moderniza todos los vuelos?? Supongo que es valido?
modernizarFlota [] = []
modernizarFlota ((origen, destino, duracion):vuelos) = (origen, destino, duracionReducida):modernizarFlota vuelos -- Me fijo el head, reduzco su duracion y repito con la tail
    where duracionReducida = duracion - ((duracion*10)/100) -- Cálculo para reducir la duracion un 10%

------------------ EJERCICIO 4 ------------------

ciudadMasConectada :: AgenciaDeViajes -> Ciudad -- Posiblemente el metodo mas basura de usar
ciudadMasConectada agencia = obtenerMaximo (contarTodasLasCiudades agencia [])
    -- Este metodo llama a contarTodasLasCiudades. Le pasa todos los vuelos y una lista vacia donde se van agregando las ciudades contadas.
    -- Luego, obtuve una lista de la forma [(Ciudad, Integer)]. Con esa lista llamo a obtener maximo que se fija la ciudad que tenga el integer mas grande.

contarTodasLasCiudades :: AgenciaDeViajes -> [Ciudad] -> [(Ciudad, Integer)] -- [(Ciudad, Integer)] lo utilizo para representar cuantas conexiones tiene cada ciudad.
contarTodasLasCiudades [] _ = []                                             -- Ej [("BsAs",2) , ("Rosario", 1) , ("Cordoba",1)] significa que BsAs se conecta con 2 ciudades, Rosario con 1, Cordoba con 1.
contarTodasLasCiudades ((origen, destino, duracion):vuelos) contadas
    | not (elem origen contadas) = (origen, contarUnaCiudad agencia origen):contarTodasLasCiudades agencia (origen:contadas) -- Si el origen de un vuelo no fue contado, lo cuento y lo agrego como contado.
    | not (elem destino contadas) = (destino, contarUnaCiudad agencia destino):contarTodasLasCiudades agencia (destino:contadas) -- Si el destino de un vuelo no fue contado, lo cuento y lo agrego como contado.
    | otherwise = contarTodasLasCiudades vuelos contadas -- Llego a este caso si el origen y el destino ya fueron contados (sigo con la lista).
    where agencia = ((origen,destino,duracion):vuelos)
    -- Este metodo llama a contarUnaCiudad para contar todas las conexiones de una ciudad.

contarUnaCiudad :: AgenciaDeViajes -> Ciudad -> Integer -- contarUnaCiudad recibe una unica ciudad y llama a ciudadesConectadas
contarUnaCiudad agencia ciudad = contarCiudadesConectadas (ciudadesConectadas agencia ciudad)
    -- Este método llama a: ciudadesConectadas (ejercicio 2) para obtener la lista de conexiones y luego
    -- Llama a contarCiudadesConectadas, que obtiene el cardinal de esa lista

contarCiudadesConectadas :: [Ciudad] -> Integer
contarCiudadesConectadas [] = 0
contarCiudadesConectadas (ciudad:ciudades) = 1 + contarCiudadesConectadas ciudades    

obtenerMaximo :: [(Ciudad, Integer)] -> Ciudad
obtenerMaximo [(ciudad, conexiones)] = ciudad
obtenerMaximo (dupla:duplas)
    | snd dupla > snd (head duplas) = obtenerMaximo (dupla:tail duplas)
    | otherwise = obtenerMaximo duplas

------------------ EJERCICIO 5 ------------------

sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar (vuelo:vuelos) origen destino
    | estaEnLaLista ciudades destino = True
    | otherwise = sePuedeLlegarConUnaEscala (vuelo:vuelos) ciudades destino
    where ciudades = destinosDeUnaCiudad (vuelo:vuelos) origen
    -- Utilizo "destinosDeUnaCiudad". Si el destino no esta en la lista que devuelve,
    -- me fijo si llego con una escala usando "sePuedeLlegarConUnaEscala"

sePuedeLlegarConUnaEscala :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Bool
sePuedeLlegarConUnaEscala (vuelo:vuelos) (unaCiudad:ciudades) destino
    | estaEnLaLista (destinosDeUnaCiudad (vuelo:vuelos) unaCiudad) destino = True
    | otherwise = False
    -- Misma lógica que "sePuedeLlegar"

estaEnLaLista :: [Ciudad] -> Ciudad -> Bool
estaEnLaLista [] ciudad = False
estaEnLaLista (unaCiudad:ciudades) ciudad
    | ciudad == unaCiudad = True
    | otherwise = estaEnLaLista ciudades ciudad
-- elem esta permitido!!

------------------ EJERCICIO 6 ------------------
-- Tiene que existir una ruta, pero me fijo si existe? y supongo que es solo con una escala como max. Preguntar por las dudas.

duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido agencia origen destino = obtenerDuracionMinima duraciones
    where duraciones = calcularDuracion agencia origen destino:obtenerDuracionesConEscala agencia origen destino

obtenerDuracionMinima :: [Duracion] -> Duracion
obtenerDuracionMinima [x] = x
obtenerDuracionMinima (duracion:ds)
    | duracion < head ds = obtenerDuracionMinima (duracion:tail ds)
    | otherwise = obtenerDuracionMinima ds

calcularDuracion :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
calcularDuracion [] _ _ = 0
calcularDuracion ((origenv, destinov, duracion):vuelos) origen destino
    | origenv == origen && destinov == destino = duracion
    | otherwise = calcularDuracion vuelos origen destino

obtenerDuracionesConEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
obtenerDuracionesConEscala agencia origen destino
    | sePuedeLlegarConUnaEscala agencia ciudades destino = calcularDuraciones agencia origen ciudades destino
    | otherwise = []
    where ciudades = destinosDeUnaCiudad agencia origen

calcularDuraciones :: AgenciaDeViajes -> Ciudad -> [Ciudad] -> Ciudad -> [Duracion]
calcularDuraciones _ _ [] _  = []
calcularDuraciones agencia origen ciudades destino =
    (calcularDuracion agencia origen (head ciudades) + calcularDuracion agencia (head ciudades) destino):calcularDuraciones agencia origen (tail ciudades) destino

------------------ EJERCICIO 7 ------------------ Sale planteandolo como nodos
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen agencia origen = visitarDestinos agencia origen destinosIniciales visitadosIniciales
    where (destinosIniciales, visitadosIniciales) = (destinosDeUnaCiudad agencia origen, [])

conocerNodos :: AgenciaDeViajes -> Ciudad -> [Ciudad] -> Ciudad -> Bool
conocerNodos ((origenv, destino, duracion):vuelos) origen visitados actual
    | actual == origen = True
    | otherwise = visitarDestinos ((origenv, destino, duracion):vuelos) origen (destinosDeUnaCiudad ((origenv, destino, duracion):vuelos) actual) (actual:visitados)

visitarDestinos :: AgenciaDeViajes -> Ciudad -> [Ciudad] -> [Ciudad] -> Bool
visitarDestinos agencia origen destinos visitados
    | destinos /= [] = conocerNodos agencia origen visitados (head destinos) || visitarDestinos agencia origen (tail destinos) visitados
    | otherwise = False

-- 1) Parto desde "ORIGEN" y quiero llegar a "ORIGEN". Me fijo los destinos de "ORIGEN"
-- 2) Encontre "ORIGEN"? Si es asi, devuelvo True. 
-- 3) Si no es asi, y ya visite **TODOS** esos destinos, entonces no puedo llegar (devuelvo False).
-- 4) Si no, me tengo que preguntar los destinos de los nodos "DESTINO" no visitados. 
        -- Entonces, marco ese nodo "DESTINO" como visitado y repito paso 1 partiendo desde "DESTINO"

-- Este metodo termina armando una serie de ORs y con que exista un destino que me lleve a "A" me da verdadero.
-- Por ejemplo tengo algo del tipo (False || False || False || True) -> True

------------------ creo que no termina hasta que conoce todo el mapa?? preguntar ------------------ 


modernizarFlotaTryhard :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlotaTryhard (vuelo:vuelos) | vuelosValidos (vuelo:vuelos) = modernizarFlotaValida (vuelo:vuelos)

modernizarFlotaValida :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlotaValida [] = []
modernizarFlotaValida ((origen, destino, duracion):vuelos) = (origen, destino, duracionReducida):modernizarFlotaValida vuelos
            where duracionReducida = duracion - ((duracion*10)/100)


vuelo1 :: Vuelo
vuelo1 = ("A", "B", 1)

vuelo2 :: Vuelo
vuelo2 = ("B", "D", 3)

vuelo3 :: Vuelo
vuelo3 = ("C", "D", 1)

vuelo4 :: Vuelo
vuelo4 = ("D", "E", 4)

vuelo5 :: Vuelo
vuelo5 = ("E", "F", 5)

vuelo6 :: Vuelo
vuelo6 = ("F", "G", 6)

vuelo7 :: Vuelo
vuelo7 = ("G", "A", 7)

vuelo8 :: Vuelo
vuelo8 = ("A", "H", 8)

vuelo9 :: Vuelo
vuelo9 = ("A", "D", 3)

vuelos = [vuelo1, vuelo2, vuelo3, vuelo4, vuelo5, vuelo6, vuelo7, vuelo8, vuelo9]

vuelosDePrueba :: [(String, String, Float)]
vuelosDePrueba =
    [("A", "B", 1.0),   -- Vuelos que permiten volver
     ("B", "C", 2.0),
     ("C", "D", 3.0),
     ("D", "A", 4.0),   -- Vuelta a "A"

     ("E", "F", 2.5),   -- Vuelos que no permiten volver
     ("F", "G", 3.0),
     ("G", "H", 4.0),   -- Sin vuelta a "E"

     ("I", "J", 5.0),   -- Vuelos con vuelta
     ("J", "I", 6.0),
     ("K", "I", 7.0),
     ("L", "I", 8.0),   -- Vuelta a "I"

     ("M", "B", 4.0),   -- Vuelos que no permiten volver
     ("N", "B", 5.0)]




