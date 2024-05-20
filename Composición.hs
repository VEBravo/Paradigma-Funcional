import GHC.Base (VecElem(Int16ElemRep))
import qualified Main as doble
-- totalDeCaracteres devuelve el total de caracteres de una lista de listas
totalDeCaracteres :: [[Char]] -> Int
totalDeCaracteres lista = sum (map length lista)

masDeTres :: [String] -> Int
masDeTres lista = length (filter mayorATres (map length lista))
    where mayorATres longPalabra = longPalabra > 3

cuadrado x =x*x
filter (even.cuadrado) [1,2,3,4,5,6,7]

f::(b->c)->(a->b)->(a->c)

