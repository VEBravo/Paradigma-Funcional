-- TIPOS
type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

-- FUNCIONES 
rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante p1 p2 = 

-- AUXILIARES
ultimosN :: Int -> [a] -> [a]
ultimosN n lista = (drop (length lista - n)) lista

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

