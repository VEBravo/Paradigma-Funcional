cubo :: Int -> Int
cubo x = x*x*x

triple :: Int -> Int
triple x = x*3

--1)
fst3 :: (Int, Int, Int) -> Int
fst3 (x, _, _) = x

snd3 :: (Int, Int, Int) -> Int
snd3 (_, y, _) = y

trd3 :: (Int, Int, Int) -> Int
trd3 (_, _, z) = z

--2)
-- Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones 
-- y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones, ej: 
-- Main> aplicar (doble,triple) 8 
-- (16,24) 
-- Main> aplicar ((3+),(2*)) 8 
-- (11,16)
aplicar :: ((Int -> Int, Int -> Int), Int) -> (Int, Int)
aplicar ((f, g), x) = (f x, g x)

cantEmpleados :: String -> Int
cantEmpleados x | x == "Acme" = 10
                | last x < head x = length x - 2
                | reverse x == x = (length x - 2) *2
                | mod (length x) 3 == 0 || mod (length x) 7 == 0 = 3
                | otherwise = 0

--3)
-- Definir la función cuentaBizarra, que recibe un par y: 
-- si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 
-- al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, 
-- devuelve el producto. Ej: 
-- Main> cuentaBizarra (5,8)
-- 40
-- Main> cuentaBizarra (8,5)
cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (x, y) | x > y = x + y
                     | y-x>10 = y - x
                     | y>x && (y-x)/=10 = y*x

--4)
-- Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un patito en el 1ro y un 7 en el
-- 2do se representan mediante el par (2,7). A partir de esto: 
-- Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 
-- Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 
-- Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y 
-- además haberse sacado al menos 7 en cada parcial. 
-- Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. 
-- La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
-- Main> (... algo ...) (5,8) 
esNotaBochazo :: Int -> Bool
esNotaBochazo nota1 = nota1<6

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = not (esNotaBochazo nota1 || esNotaBochazo nota2)

promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = nota1 + nota2 >= 15 && nota1 >= 7 && nota2 >=7






