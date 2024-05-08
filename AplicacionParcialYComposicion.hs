import Graphics.Win32 (eS_MULTILINE)
-- (1)
-- Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
-- Main> siguiente 3
-- 4
siguiente :: Int -> Int
siguiente = (1 +)

-- (2)
-- Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
-- Main> mitad 5
-- 2.5
mitad :: Float -> Float
mitad = (/ 2)

-- (3)
-- Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
-- Main> inversa 4
-- 0.25
-- Main> inversa 0.5
-- 2.0
inversa :: Float -> Float
inversa = (1 /)

-- (4)
-- Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
-- Main> triple 5 
-- 15
triple :: Float -> Float
triple = (3 *)

cuadrado :: Int -> Int
cuadrado x = x*x

-- (5)
-- Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario. 
-- Main> esNumeroPositivo (-5)
esNumeroPositivo :: Float -> Bool
esNumeroPositivo = (> 0)

-- (6)
-- Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = ((0 ==).mod x) y

-- (7)
-- Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
esBisiesto :: Int -> Bool
esBisiesto x = esMultiploDe 400 x || (esMultiploDe 4 x && not (esMultiploDe 100 x))

-- (8)
-- Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
-- Main> inversaRaizCuadrada 4 
-- 0.5 
-- Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.
inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt

-- (9)
-- Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
-- Main> incrementMCuadradoN 3 2 
-- 11 
-- Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición.
incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN n m = ((m +) . cuadrado ) n

-- Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
-- Main> esResultadoPar 2 5 
-- True 
-- Main> esResultadoPar 3 2
-- False 
-- Nota Obvia: Resolverlo utilizando aplicación parcial y composición.

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n = even . (n^)