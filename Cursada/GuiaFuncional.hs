import Data.IntSet (IntSet)
-- EJERCICIO 1
-- Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3
esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = mod x 3==0

-- EJERCICIO 2
-- Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x==0

-- EJERCICIO 3
-- Definir la función cubo/1, devuelve el cubo de un número.
cubo :: Float -> Float
cubo x = x*x*x

-- EJERCICIO 4
-- Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area :: Float -> Float -> Float
area x y = x*y

-- EJERCICIO 5
-- Definir la función esBisiesto/1, indica si un año es bisiesto. 
-- (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2
esBisiesto :: Int -> Bool
esBisiesto x = esMultiploDe 400 x || (esMultiploDe 4 x && not (esMultiploDe 100 x))

--EJERCICIO 6
celsiusToFahr :: Float -> Float
celsiusToFahr x = x*9/5 + 32

--EJERCICIO 7
fahrToCelsius :: Float -> Float
fahrToCelsius x = (x - 32) / (9/5)

--EJERCICIO 8
haceFrio :: Float -> Bool
haceFrio x = fahrToCelsius x < 8

--EJERCICIO 9
-- mcm :: Int -> Int -> Int
-- mcm x y = (x*y) / (gcd x y)
--PREGUNTARLE AL PROFE

--EJERCICIO 10
dispersion :: Int -> Int -> Int -> Int
dispersion x y z = max (max x y) z - min (min x y) z

diasParejos :: Int -> Int -> Int -> Bool
diasParejos x y z = dispersion x y z < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos x y z = dispersion x y z > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales x y z = not (diasParejos x y z || diasLocos x y z)

sumaImpares :: Int -> Int 
sumaImpares 0 = 0
sumaImpares n = n * 2 - 1 + sumaImpares (n - 1)

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto n = n == sumaImpares n

