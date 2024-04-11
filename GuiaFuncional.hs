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
