sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

len [] = 0
len (x:xs) = 1 + len xs

--2)----------------------------------
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 

promedioFC :: [Int] -> Float
promedioFC lista = fromIntegral(sum2 lista) / fromIntegral(len lista)

momFC :: Int -> Int
momFC i = frecuenciaCardiaca !! round(fromIntegral i/10)

hastaMomentoFC :: Int -> [Int]
hastaMomentoFC n = take (round(fromIntegral n/10)) frecuenciaCardiaca

esCapicua :: [String] -> Bool
esCapicua lista = concat lista == (reverse.concat) lista

-- 4) --------
registro = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))
cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos | (sum.snd.fst) registro > (sum.snd.snd) registro = (fst.fst) registro
                      | otherwise = (fst.snd) registro

masLlamadas :: String
masLlamadas | (length.snd.fst) registro > (length.snd.snd) registro = (fst.fst) registro
            | otherwise = (fst.snd) registro

-- Orden Superior
existAny :: (Int, Int, Int) -> (Int -> Bool) -> Bool
existAny (a,b,c) f = f a || f b || f c

mejor :: (Int->Int) -> (Int->Int) -> Int -> Int
mejor f g n | f n > g n = f n
            | otherwise = g n

aplicarPar :: (Int->Int) -> (Int,Int) -> (Int,Int)
aplicarPar f p = ((f.fst)p,(f.snd)p)

-- Ord Sup con Listas
esMult :: Int -> Int -> Bool
esMult a b = mod a b == 0

esMultiploDeAlg :: Int -> [Int] -> Bool
esMultiploDeAlg n [] = False
esMultiploDeAlg n (x:xs) = esMult x n || esMultiploDeAlg n xs

promedioListas :: [[Int]] -> [Float]
promedioListas lista = map promedioFC lista 

promediosSinAplazos :: [[Int]] -> [Float]
promediosSinAplazos = (filter (>4).map promedioFC)

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

exists:: (a->Bool) -> [a] -> Bool
exists = any

aplicarF :: [a -> b] -> a -> [b]
aplicarF funciones n = map (\f -> f n) funciones

sumaF :: Num b => [a->b] -> a -> b
sumaF funciones n = sum(aplicarF funciones n)

--12)
subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad n lista = map (\nro -> (min 12.(+n)) nro) lista

flimitada :: (Int->Int) -> Int -> Int
flimitada f = (min 12.max 0.f)

cambiarHabilidad :: (Int->Int) -> [Int] -> [Int]
cambiarHabilidad f lista = map (\nro -> flimitada f nro) lista

primerosPare :: [Int] -> [Int]
primerosPare = takeWhile even

primerosDivisore :: Int -> [Int] -> [Int]
primerosDivisore n lista = takeWhile (\nro -> ((==0).mod n) nro) lista

huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe [] [] nro = False
huboMesMejorDe (x:xs) (y:ys) nro = x-y > nro || huboMesMejorDe xs ys nro

-- 17)
crecAnual :: Int -> Int
crecAnual edad | 1<=edad && edad<=9 = 24 - (edad*2)
               | edad<=15 = 4
               | edad<=17 = 2
               | edad<=19 = 1
               | otherwise = 0

crecEntreEdades :: Int -> Int -> Int
crecEntreEdades e1 e2 = (sum.map crecAnual)  [e1..e2-1]

altEnUnAnio :: Int -> [Int] -> [Int]
altEnUnAnio edad = map (\alt -> alt + crecAnual edad)

alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edad edades = map (\e -> crecEntreEdades edad e + altura) edades

-- 18)
sumarLista :: [Int] -> Int
sumarLista = foldl1 (+)

productoria :: [Int] -> Int
productoria = foldl1 (*)

dispresion :: [Int] -> Int
dispresion lista = maximum lista - minimum lista