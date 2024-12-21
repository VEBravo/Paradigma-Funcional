import Text.Show.Functions

siguiente :: Int -> Int
siguiente = (+) 1

mitad :: Float -> Float
mitad = (/ 2)

inversa :: Float -> Float
inversa = (1 /)

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a = ((== 0). mod a)

esBisiesto :: Int -> Bool
esBisiesto anio = (esMultiploDe anio 400) || (esMultiploDe anio 4) && not(esMultiploDe anio 100)

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada n = (sqrt.inversa) 16

incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN m n = ((+ m).(n*))n

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n m = even(n^m)