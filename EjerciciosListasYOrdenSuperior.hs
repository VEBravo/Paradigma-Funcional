sumaLista :: [Int] -> Int
sumaLista = sum

--1)
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
promedioFrecuenciaCardiaca :: [Int]-> Float
promedioFrecuenciaCardiaca lista =  fromIntegral (sum lista) / fromIntegral (length lista)

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto x = frecuenciaCardiaca !! (div x 10)

frecuenciaHastaMomento :: Int -> [Int]
frecuenciaHastaMomento momento = take (div momento 10 + 1) frecuenciaCardiaca

--2)
esCapicua :: [String] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

--3)
duracionLlamadas :: ((String,[Int]), (String,[Int]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]), ("horarioNormal",[10,5,8,2,9,10]))
cuandoHabloMasMinutos :: ((String,[Int]), (String,[Int])) -> String
cuandoHabloMasMinutos ((reduc,listaReduc), (normal,listaNorm)) | sum listaReduc > sum listaNorm = reduc
                                                               | sum listaReduc < sum listaNorm = normal

cuandoHizoMasLlamadas :: ((String,[Int]), (String,[Int])) -> String
cuandoHizoMasLlamadas ((reduc,listaReduc), (normal,listaNorm)) | length listaReduc > length listaNorm = reduc
                                                               | length listaReduc < length listaNorm = normal

-- ORDEN SUPERIOR
--(1)
-- Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True si existe algún elemento de la tupla que haga verdadera la función. 
-- Main> existsAny even (1,3,5) 
-- False 

-- Main> existsAny even (1,4,7) 
-- True 
-- porque even 4 da True 

-- Main> existsAny (0>) (1,-3,7) 
-- True 
-- porque even -3 es negativo 

existsAny :: (Int -> Bool) -> (Int,Int,Int) -> Bool
existsAny f (a,b,c) = f a || f b || f c

esMayorA2 :: Int -> Bool
esMayorA2 x = x > 2

-- (2)
-- Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto. P.ej. 
-- Main> mejor cuadrado triple 1 
-- 3 
-- (pues triple 1 = 3 > 1 = cuadrado 1) 

-- Main> mejor cuadrado triple 5 
-- 25 
-- (pues cuadrado 5 = 25 > 15 = triple 5) 
-- Nota: No olvidar la función max. 

cubo :: Int -> Int
cubo x = x*x*x

cuadrado :: Int -> Int
cuadrado x = x*x

mejor :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejor f1 f2 n = devuelveMayor (f1 n) (f2 n)

devuelveMayor :: Int -> Int -> Int
devuelveMayor a b | a > b = a
                  | b > a = b

-- (3)
-- Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par. P.ej. 
-- Main> aplicarPar doble (3,12) 
-- (6,24) 

-- Main> aplicarPar even (3,12) 
-- (False, True) 

-- Main> aplicarPar (even . doble) (3,12) 
-- (True, True) 

aplicarPar :: (Int -> Int) -> (Int,Int) -> (Int,Int)
aplicarPar f (n1,n2) = (f n1, f n2)

-- (4)
-- Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor. P.ej. 
-- Main> parDeFns even doble 12 
-- (True, 24) 

parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns f g x = (f x, g x)

-- Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista. P.ej. 
-- Main> esMultiploDeAlguno 15 [2,3,4] 
-- True, 
-- porque 15 es múltiplo de 3 

-- Main> esMultiploDeAlguno 34 [3,4,5] 
-- False 
-- porque 34 no es múltiplo de ninguno de los 3 Nota: Utilizar la función any/2. 

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x==0 --Si el segundo es multiplo del primero

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno n lista = any esMultiploDe n lista

