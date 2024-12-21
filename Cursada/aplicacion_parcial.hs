import System.Win32 (xBUTTON1)
doble x = 2*x

losDosPrimeros :: [a] -> [a]
losDosPrimeros = take 2
-- Puedo hacer map losDosPrimeros ["Hola","Como","Estas"] y me devuelve ["Ho","Co","Es"]
-- O bien podría hacer map (take 2) ["Hola","Como","Estas"] y me devolveria lo mismo

-- Otra opcion utilizando aplicacion parcial seria

losDosPrimerosInvertidos :: [a] -> [a]
losDosPrimerosInvertidos = ((take 2).reverse)
-- Y llamar a la funcion para el map, o bien hacer un map directamente utilizando aplicacion parcial
-- map ((take 2).reverse) ["Hola","Como","Estas"]

--Ejemplo, filtrar los numeros mayores a 10, multiplicados por 5 sumado 1 y dividido 10

laMitadMasUno = (+1).(/2)
-- ghci> laMitadMasUno 5
-- 3.5

-- Si quiero mapear el triple de una lista de numeros


f algo = algo 5
