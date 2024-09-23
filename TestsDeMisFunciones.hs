module TestsDeMisFunciones where

import Test.HUnit
import MisFunciones

run = runTestTT testsHt

runPares = runTestTT testsPares

testsHt = test [ 
    "Caso base 0:  fib 0" ~: (fib 0) ~?= 0,
    "Caso base 1:  fib 0" ~: (fib 1) ~?= 1,
    "Caso 8:  fib 8" ~: (fib 8) ~?= 21  
    ]


testsPares = test [ 
    "Caso sin pares: pares [1,3,7]" ~: (pares [1,3,7]) ~?= [],
    "Caso 1,2,3,4: pares [1,2,3,4]" ~: (pares [1,2,3,4]) ~?= [2,4],
    "Caso pares e impares sin repetidos [1,2,3,4]" ~: (pares [2,6,3,4]) ~?= [2,6,4],
    "Caso pares e impares sin repetidos [1,2,3,4]" ~: (pares [2,6,6,6,1]) ~?= [2,6]
    ]
