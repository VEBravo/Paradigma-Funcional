import qualified Data.Tuple as odd
-- Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta. 
-- Main> calcular (4,5)
-- (8,6) 
-- Main> calcular (3,7)
-- (3,8) 
-- Nota: Resolverlo utilizando aplicación parcial y composición. 
calcular :: (Int,Int) -> (Int,Int)
calcular tupla | (even.fst) tupla && (odd.snd) tupla = (((2*).fst).((1+).snd)) tupla 
               | (odd.snd) tupla = ((1+).snd) tupla
               | 