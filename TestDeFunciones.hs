module TestDeMisFunciones where

import Data.List
import Test.HUnit
import Solucion

run = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "Test 1: vuelos vacia" ~: vuelosValidos [] ~?= True,
    "Test 2: vuelos valido con un elemento valido" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
    "Test 3: vuelos valido con un elemento invalido" ~: vuelosValidos [("BsAs", "BsAs", 5.0)] ~?= False,
    "Test 4: vuelos valido con mas de un elemento repetido" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("BsAs", "Rosario", 6.0)] ~?= False,
    "Test 5: vuelos valido con mas de un elemento distitno" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("BsAs", "Córdoba", 6.0)] ~?= True
    ]

testsEjciudadesConectadas = test [
    "Test 1: ciudad conectada con un elemento" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    "Test 2: ciudad conectada con elementos repetidos" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Rosario","BsAs",5.0)] "Rosario" ~?= ["BsAs"],
    "Test 3: ciudad conectada con elementos distintos" ~: expectPermutacion (ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Rosario","Usuhaia",5.0)] "Rosario") ["BsAs","Usuhaia"]
    ]

testsEjmodernizarFlota = test [
    "Test 1: flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)],
    "Test 2: flota modernizada con mas de un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0),("BsAs","Córdoba",20.0)] ~?= [("BsAs", "Rosario", 9.0),("BsAs","Córdoba", 18.0)]
    ]

testsEjciudadMasConectada = test [
    "Test 1: ciudad mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario",
    "Test 2: ciudad mas conectada donde pueden ser todas 1" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Córdoba", "Salta", 7.0)]) ["BsAs","Rosario","Córdoba","Salta"],
    "Test 3: ciudad mas conectada donde pueden ser todas 2" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0),("BsAs","Córdoba",9.0)]) ["Rosario","Córdoba","BsAs"],
    "Test 4: ciudad mas conectada donde pueden ser algunas pero no todas" ~: expectAny (ciudadMasConectada [("Rosario", "Córdoba", 10.0), ("Salta", "Chapadmalal", 19.0),("Córdoba","Rosario",9.0)]) ["Rosario","Córdoba","Salta","Chapadmalal"]
    ]

testsEjsePuedeLlegar = test [
    "Test 1: Se puede llegar caso verdadero con una escala 1" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Test 2: Se puede llegar caso verdadero con una escala 2" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0), ("Rosario", "Córdoba", 5.0)] "BsAs" "Córdoba" ~?= True,
    "Test 3: Se puede llegar caso verdadero con una escala 3" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0), ("Rosario", "Córdoba", 5.0)] "Córdoba" "Rosario" ~?= True,
    "Test 4: Se puede llegar caso verdadero con una escala 4" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0), ("Salta","Córdoba",8.0), ("Rosario", "Córdoba", 5.0)] "Salta" "BsAs" ~?= True,
    "Test 5: Se puede llegar caso verdadero sin escalas" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("BsAs", "Córdoba", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Test 6: Se puede llegar caso falso 1" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0), ("Rosario", "Córdoba", 5.0), ("Salta","Córdoba",8.0)] "Salta" "Rosario" ~?= False,
    "Test 7: Se puede llegar caso falso 2" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("BsAs", "Córdoba", 8.0)] "Córdoba" "BsAs" ~?= False
    ]

testsEjduracionDelCaminoMasRapido = test [
    "Test 1: duración del camino mas rapido con una escala 1" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 10.0,
    "Test 2: duración del camino mas rapido con una escala 2" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0), ("Rosario", "Córdoba", 5.0)] "BsAs" "Córdoba" ~?= 10.0,
    "Test 3: duracion del camino más rapido sin escala 1" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Rosario" ~?= 5.0,
    "Test 4: duracion del camino más rapido sin escala 2" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Córdoba", "BsAs", 8.0)] "Córdoba" "BsAs" ~?= 8.0
    ]

testsEjpuedoVolverAOrigen = test [
    "Test 1: puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True
    ]

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)

expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
