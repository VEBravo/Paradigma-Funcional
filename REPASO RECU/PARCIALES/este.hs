-- FUNCIONES UTILES
-- Cycle        ->      Toma lista y la repite infinita
-- Words        ->      Divide palabras
-- Unwords      ->      Une lista de palabras con espacios
-- takeWhile    ->      Mientras se cumpla
-- dropWhile    ->      Mientras se cumpla
-- zipWith      ->      zipWith (+) [1,2,3] [4,5,6]
-- nub          ->      Elmina duplicados import Data.List (nub)
-- partition f  ->      Crea dos listas en una tupla con los que cumplen y los que no
-- subsequences ->      Combinatoria
-- permutations ->      Permutaciones
-- group        ->      Agrupa consecutivos
-- elemIndex    ->      Just 2 → Maybe (-1)
-- elemIndices  ->      elemIndices 'a' "banana" [1,3,5]
-- foldl :: (b -> a -> b) -> b -> [a] -> b



-- TEORICOS
{- RTA: Tomo como que la funcion recibiria una lista de hechizos y una lista de postres. En este caso, lazy evaluation nos ayudaria solo
    si es falso ya que para saber si todos quedan listos, deberia utilizar un any para la lista de hechizos y un all para la lista de postres
    entonces tendria que leer la lista entera de postres, si es verdadero que ese hechizo los deja listos (algo imposible si es infinita). 
    Por otro lado, si todos los hechizos devuelven por lo menos un postre que no este listo,  entonces devolveria False. Ya que todos los 
    all devolverian False. Y el any tambien daria False -}

{-  RTA: En este caso, lazy evaluation no nos ayudaria ya que para saber el mejor hechizo debe leer la lista entera hasta que quede uno solo. Por lo que
    si es infinita, por mas que vayas sacando hechizos, nunca va a quedar uno solo. No hay ningun caso en el que se pueda encontrar el mejor hechizo.-}