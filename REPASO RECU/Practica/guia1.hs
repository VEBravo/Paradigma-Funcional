esMultiploDeTres :: Int -> Bool
esMultiploDeTres n = mod n 3 == 0

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0

cubo :: Int -> Int
cubo n = n*n*n

-- Dispersion
max3 :: Int -> Int -> Int -> Int
max3 a b c = max c (max a b)

min3 :: Int -> Int -> Int -> Int
min3 a b c = min c (min a b)

dispersion :: Int -> Int -> Int -> Int
dispersion a b c = max3 a b c - min3 a b c

diasParejos :: Int -> Int -> Int -> Bool
diasParejos a b c = dispersion a b c < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos a b c = dispersion a b c > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales a b c = not (diasLocos a b c) && not (diasParejos a b c)

-- Pinos
pesoPino :: Float -> Float
pesoPino altura | altura <= 3 = altura * 300
                | otherwise = 900 + (altura - 3)*200

esPesoUtil :: Float -> Bool
esPesoUtil peso = peso > 400 && peso < 1000

sirvePino :: Float -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

