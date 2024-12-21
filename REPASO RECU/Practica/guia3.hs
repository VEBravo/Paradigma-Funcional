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
aplicar :: ((Int->Int),(Int->Int)) -> Int -> (Int,Int)
aplicar (f1,f2) a = (f1 a, f2 a)

--3)
cuentaBizarra :: (Int,Int) -> Int
cuentaBizarra (a,b) | a > b = a + b
                    | (b-a)>10 = b -a 
                    | (b>a) = a*b 

--4)
esNotaBochazo :: Int -> Bool
esNotaBochazo = (>) 6

aprobo :: (Int,Int) -> Bool
aprobo (a,b) = not(esNotaBochazo a || esNotaBochazo b)

promociono :: (Int,Int) -> Bool
promociono (a,b) = a + b >= 15 && a >= 7 && b >= 7

aproboPrimerParcial :: (Int,Int) -> Bool
aproboPrimerParcial (a,_) = (not.esNotaBochazo) a

--5)
notasFinales :: ((Int,Int),(Int,Int)) -> (Int,Int)
notasFinales ((p1,p2),(r1,r2)) = (max p1 r1, max p2 r2)

recuperoDeGusto :: ((Int,Int),(Int,Int)) -> Bool
recuperoDeGusto notas = promociono(fst notas) && ((fst.snd) notas /= -1 || (snd.snd) notas /= -1)

--6)
esMayorDeEdad :: (String,Int) -> Bool
esMayorDeEdad = ((>= 21).snd)

--7)
calcular :: (Int,Int) -> (Int,Int)
calcular (a,b) | even a && odd b = (a*2,b+1)
               | even a = (a*2,b)
               | odd b = (a,b+1)
               | otherwise = (a,b)