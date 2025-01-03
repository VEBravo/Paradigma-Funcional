import System.Directory (createDirectoryIfMissing)
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
esMultiploDeAlguno n lista = any (esMultiploDe n) lista

-- 2
-- Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento. P.ej. 
-- Main> promedios [[8,6],[7,9,4],[6,2,4],[9,6]] 
-- [7,6.67,4,7.5] 
-- Nota: Implementar una solución utilizando map/2

promedios :: [[Float]] -> [Float]
promedios lista = map promedio lista

promedio :: [Float] -> Float
promedio lista = sum lista / fromIntegral (length lista)

-- 3
-- Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, 
-- excluyendo los que sean menores a 4 que no se cuentan. P.ej. 
-- Main> promediosSinAplazos [[8,6],[6,2,6]] 
-- [7,6] 
-- Nota: Implementar una solución utilizando map/2. 

promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos listaDeListas = filter (4<) (promedios listaDeListas)

-- 4
-- Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno. P.ej. 
-- Main> mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]] 
-- [8,9,6,10]. 
-- Ayuda: Utilizar la función predefinida maximum/1. 

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum 

--5
-- Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más. P.ej. 
-- Main> aprobo [8,6,2,4] 
-- False 
-- Main> aprobo [7,9,6,8] 
-- True 
-- Ayuda: Utilizar la función predefinida minimum/1. 

aprobo :: [Float] -> Bool
aprobo = all (>6) 

--6
-- Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron. P.ej. 
-- Main> aprobaron [[8,6,2,4],[7,9,6,7],[6,2,4,2],[9,6,7,10]] 
-- [[7,9,6,7],[9,6,7,10]] 
-- Ayuda: usar la función aprobó/1.

aprobaron :: [[Float]] -> [[Float]]
aprobaron = filter aprobo

--7
-- Definir la función divisores/1, que recibe un número y devuelve la lista de divisores. P.ej. 
-- Main> divisores 60 
-- [1,2,3,4,5,6,10,12,15,20,30,60] 
-- Ayuda: para calcular divisores n alcanza con revisar los números entre 1 y n.

esDivisor :: Int -> Int -> Bool
esDivisor numero divisor = numero `mod` divisor == 0

divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]

-- 8
-- Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún elemento de la lista. P.ej. 
-- Main> exists even [1,3,5] 
-- False 
-- Main> exists even [1,4,7] 
-- True 
-- porque even 4 da True 

exists :: (a -> Bool) -> [a] -> Bool
exists = any

--9
-- Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo. 
-- Main> hayAlgunNegativo [2,-3,9] (…algo…) 
-- True 

hayAlgunNegativo :: [Int] -> a -> Bool
hayAlgunNegativo lista algo = any (<0) lista

--10
-- Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, devuelve la lista del resultado de aplicar las funciones al valor. P.ej. 
-- Main> aplicarFunciones[(*4),(+3),abs] (-8) 
-- [-32,-5,8] 
-- Si pongo:
-- Main> aplicarFunciones[(*4),even,abs] 8 
-- da error. ¿Por qué? 
aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones listaFunciones valor = map (\f->f valor) listaFunciones


-- Definir la función sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número. P.ej.
-- Main> sumaF[(*4),(+3),abs] (-8) 
-- -29 
sumaF :: Num b => [a -> b] -> a -> b
sumaF lista num = sum (aplicarFunciones lista num)

-- Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual 
-- (como el Hattrick o el ManagerZone). En un momento le llega la información sobre 
-- la habilidad de cada jugador de un equipo, que es un número entre 0 y 12, y 
-- la orden de subir la forma de todos los jugadores en un número entero; p.ej., subirle 2 la forma a cada jugador.
--  Ahora, ningún jugador puede tener más de 12 de habilidad; si un jugador tiene 11 y la orden es subir 2, pasa a 12, no a 13; si estaba en 
--  12 se queda en 12. Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar) y una lista de números, y 
--  le suba la habilidad a cada jugador cuidando que ninguno se pase de 12. P.ej. 
-- Main> subirHabilidad 2 [3,6,9,10,11,12] 
-- [5,8,11,12,12,12] 

subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad num = map (sumaMenorA12 num)

sumaMenorA12 :: Int -> Int -> Int
sumaMenorA12 x y | 12-x <= y = x + y
                 | otherwise = 12

--13Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador 
-- según una función que recibe la vieja habilidad y devuelve la nueva. Armar: una función 
-- flimitada que recibe una función f y un número n, y devuelve f n garantizando que quede 
-- entre 0 y 12 (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12). 
-- P.ej. 
-- Main> flimitada (*2) 9 
-- 12 
-- pues 9*2 = 18 > 12 

-- Main> flimitada (+(-4)) 3 
-- 0 
-- pues 3-4 = -1 < 0 

-- Main> flimitada (*2) 5 
-- 10 
-- pues 5*2 = 10 que está en rango Hacerlo en una sola línea y sin guardas. Ayuda: usar min y max.

-- Definir una función cambiarHabilidad/2, que reciba una función f y una lista de habilidades, y devuelva el resultado de 
-- aplicar f con las garantías de rango que da flimitada. 

-- P.ej. 
-- Main> cambiarHabilidad (*2) [2,4,6,8,10] 
-- [4,8,12,12,12] 

-- Usar cambiarHabilidad/2 para llevar a 4 a los que tenían menos de 4, dejando como estaban al resto. 

-- P.ej. 
-- Main> cambiarHabilidad ... [2,4,5,3,8] 
-- [4,4,5,4,8] 
-- Lo que hay que escribir es completar donde están los puntitos.
fLimitada :: (Int -> Int) -> Int -> Int
fLimitada f n | f n < 0 = 0
              | f n > 12 = 12
              | n < 4 = 4
              | otherwise = f n

cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
cambiarHabilidad f lista = map (fLimitada f) lista

primerosPares :: [Int] -> [Int]
primerosPares = takeWhile even

primerosDivisoresDeN :: [Int] -> Int -> [Int]
primerosDivisoresDeN lista n = takeWhile (esDivisor n) lista

primerosNoDivisoresDeN :: [Int] -> Int -> [Int]
primerosNoDivisoresDeN lista n = takeWhile (not . esDivisor n) lista
--(16)
huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe ingresos egresos n = any (> n) (diferenciaIE ingresos egresos)

diferenciaIE :: [Int] -> [Int] -> [Int]
diferenciaIE ingresos egresos = map (\t->fst t - snd t) (zip ingresos egresos)
--(17)
crecimientoAnual :: Int -> Int
crecimientoAnual edad | 1 < edad && edad < 10 = 24-edad*2
                      | edad <= 15 = 4
                      | edad <= 17 = 2
                      | edad <= 19 = 1
                      | otherwise = 0

crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades e1 e2 = sum(map crecimientoAnual [e1..e2])

alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad = map (\a->a + crecimientoAnual edad)

alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edadA = map (\e-> crecimientoEntreEdades edadA e + altura)

--(18)
dispersion :: [Int] -> (Int,Int)
dispersion lista = (foldl (\a -> max a) 0 lista , foldl (\a -> min a) 1000 lista )


