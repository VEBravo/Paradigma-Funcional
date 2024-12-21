type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre
type Rima = Palabra -> Palabra -> Bool
type Conjugacion = Verso -> Verso -> Bool
type Patron = Estrofa -> Bool
type PatronCombindado = Patron -> Patron -> Estrofa -> Bool

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

-- Rima asonante: se cumple cuando las dos últimas vocales de la palabra coinciden. Por ejemplo: parcial - estirar
esRimaAsonante :: Rima
esRimaAsonante = cumplen ((take 2).(filter esVocal).reverse) (==)

-- Rima consonante: se cumple cuando las tres últimas letras de la palabra coinciden. Por ejemplo: función - canción
esRimaConsonante :: Rima
esRimaConsonante = cumplen ((take 3).reverse) (==)

rimanPalabras :: Rima
rimanPalabras p1 p2 | p1 == p2 = False
                    | esRimaAsonante p1 p2 || esRimaConsonante p1 p2 = True
                    | otherwise = False
-- Por medio de rimas: dos versos se conjugan con rima cuando logran rimar las últimas palabras de cada uno. Por ejemplo:
conjugaRimas :: Conjugacion
conjugaRimas = cumplen (last.words) rimanPalabras
-- Haciendo anadiplosis: sucede cuando el segundo verso comienza con la misma palabra con la que termina el primero
conjuganAnadiplosis :: Conjugacion
conjuganAnadiplosis v1 v2 = (last.words) v1 == (head.words) v2
-- Simple: es un patrón en el que riman 2 versos, especificados por su posición en la estrofa.
tienePatronSimple :: Int -> Int -> Patron
tienePatronSimple num1 num2 estrofa = conjugaRimas ((!!) estrofa (num1-1)) ((!!) estrofa (num2-1))
-- Esdrújulas: Todos los versos terminan con palabras en esdrújula. Diremos que una palabra es esdrújula cuando la antepenúltima vocal está acentuada. Un ejemplo de este patrón sería:
esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde.(!! 2).reverse.filter esVocal
--
tienePatronEsdrujula :: Patron
tienePatronEsdrujula = all (esEsdrujula.last.words)
-- Anáfora: Todos los versos comienzan con la misma palabra. Por ejemplo:
tienePatronAnafora :: Patron
tienePatronAnafora estrofa = all ((primerPalabraEstrofa estrofa ==).primerPalabraVerso) estrofa

primerPalabraEstrofa :: Estrofa -> Palabra
primerPalabraEstrofa estrofa = (head.words.head) estrofa

primerPalabraVerso :: Verso -> Palabra
primerPalabraVerso verso = (head.words) verso

-- Cadena: Es un patrón que se crea al conjugar cada verso con el siguiente, usando siempre la misma conjugación. 
-- La conjugación usada es elegida por el artista mientras está rapeando. Por ejemplo, una cadena de anadiplosis sería:
tienePatronCadena :: Conjugacion -> Estrofa -> Bool
tienePatronCadena conjugacion estrofa = conjuganVersosEstrofa conjugacion estrofa 1 2 && conjuganVersosEstrofa conjugacion estrofa 2 3 && conjuganVersosEstrofa conjugacion estrofa 3 4

conjuganVersosEstrofa :: Conjugacion -> Estrofa -> Int -> Int -> Bool
conjuganVersosEstrofa conjugacion estrofa vn1 vn2 = conjugacion (estrofa !! (vn1-1)) (estrofa !! (vn2-1))

-- CombinaDos: Dos patrones cualesquiera se pueden combinar para crear un patrón más complejo, y decimos que una estrofa lo cumple cuando cumple ambos patrones a la vez. 
-- Por ejemplo, si contemplamos el patrón combinado de esdrújulas y anáfora, una estrofa que cumpliría podría ser

combinaDos :: PatronCombindado
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

aabb :: Estrofa -> Bool
aabb estrofa = tienePatronSimple 1 2 estrofa && tienePatronSimple 3 4 estrofa

abab :: Estrofa -> Bool
abab estrofa = tienePatronSimple 1 3 estrofa && tienePatronSimple 2 4 estrofa

abba :: Estrofa -> Bool
abba estrofa = tienePatronSimple 1 4 estrofa && tienePatronSimple 2 3 estrofa

hardcore :: Estrofa -> Bool
hardcore estrofa = tienePatronCadena conjugaRimas estrofa && tienePatronEsdrujula estrofa

-- ¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore? 
-- No se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore ya que para tener el patrón rimas es necesario que se evalúe si todos los versos cumplen con la condición de rimar
-- Como una estrofa es una lista, tener versos infinitos significa tener una lista infinita. No podriamos evaluar que todos los versos cumplan con la condición ya que nunca se temrinarian de evaluar todos los versos.

-- ¿Y el aabb?
-- Si, podriamos saber si cumple con el patron ya que este patrón evaluará unicamente los primeros 4 versos, y como Haskell utiliza evaluación diferida, a diferencia de otros programas, en vez de recorrer la lista
-- completa siempre, si detecta que se cumple la condición necesaria dejará de recorrer la lista, por lo que únicamente evalua los primeros cuatro versos y el resto los descarta, de forma que no importa
-- que la lista sea infinita.

-- Por ahora pudimos identificar las siguientes variables significativas de una puesta en escena: si el público está exaltado o no, la potencia (un número), además de, claro está, la estrofa del freestyle 
-- (una sola, la puesta es por estrofa) y el artista.
-- Además nos dimos cuenta que en cada puesta en escena cada artista utiliza un estilo distinto, dependiendo del mensaje que quiere transmitir, que altera la puesta en escena original. Identificamos los siguientes casos:

data PuestaEnE = UnaPuestaEnE {
    publicoExaltado :: Bool,
    potencia :: Float,
    estrofa :: Estrofa,
    artista :: Artista
} deriving (Show, Eq)

type Estilo = PuestaEnE -> PuestaEnE
-- Gritar: aumenta la potencia en un 50%
gritar :: Estilo
gritar puesta = aumentarPotencia puesta 50

aumentarPotencia :: PuestaEnE -> Int -> PuestaEnE
aumentarPotencia puesta porcentaje = puesta {potencia = (potencia puesta) + (potencia puesta)*((fromIntegral porcentaje)/100)} 
-- Responder un acote: conociendo su efectividad, aumenta la potencia en un 20%, y además el público queda exaltado si la respuesta fue efectiva, sino no lo queda.
efectiva :: Bool
efectiva = True

noEfectiva :: Bool
noEfectiva = False

responderAcote :: Bool -> Estilo
responderAcote efectividad puesta | efectiva = (aumentarPotencia (puesta {publicoExaltado = True}) 20)  
                                  | noEfectiva = aumentarPotencia puesta 20

-- Tirar técnicas: se refiere a cuando el artista deja en evidencia que puede lograr algún patrón en particular, aumenta la potencia en un 10%, además el público se exalta si la estrofa cumple con dicho patrón, sino no.
tirarTecnicas :: Patron -> Estilo
tirarTecnicas patron puesta | patron (estrofa puesta) = (aumentarPotencia (puesta {publicoExaltado = True}) 10)  
                            | otherwise = aumentarPotencia puesta 10

-- Hacer que un artista se tire un freestyle a partir de la estrofa que quiere decir y el estilo que le quiera dar a su puesta en escena. Para ello se parte siempre de una puesta base que tiene potencia 1
--  y el público tranquilo, la que luego varía según el estilo utilizado.
-- El resultado de que un artista se tire un freestyle es una puesta en escena.

tirarFreestyle :: Artista -> Estrofa -> Estilo -> PuestaEnE
tirarFreestyle artista estrofa estilo = estilo (UnaPuestaEnE False 1 estrofa artista)

-- Nos contaron que cada jurado define sus propios criterios para puntuar una puesta en escena, estos criterios se basan en alguna condición que debe cumplir la puesta y 
-- un puntaje que se le otorga si la cumple. Para que nos demos una idea, nos comentaron algunos ejemplos de estos criterios: que el freestyle cumpla con un patrón determinado, 
-- si el público está exaltado, si la potencia cumple algún requisito, etc.
type Puntaje = Float
type Jurado = [(CriterioCompleto)]
type CriterioCompleto = (PuestaEnE -> Bool,Float)
type Criterio = PuestaEnE -> Bool

-- Si el freestyle (estrofa de la puesta) cumple con el patrón aabb, entonces suma 0.5 punto
criterioAabb :: Criterio
criterioAabb puesta = aabb (estrofa puesta)
--  Si el freestyle (estrofa de la puesta) cumple con el patrón combinado de esdrújulas y simple entre 1 y 4, entonces suma 1 punto
criterioCombinado :: Criterio
criterioCombinado puesta = combinaDos tienePatronEsdrujula (tienePatronSimple 1 4) (estrofa puesta)

--  Si el público está exaltado, entonces suma 1 punto
criterioPublicoExaltado :: Criterio
criterioPublicoExaltado puesta = publicoExaltado puesta

-- Si tiene una potencia mayor a 1.5, entonces suma 2 puntos
criterioPotencia :: Criterio
criterioPotencia puesta = (potencia puesta) > 1.5

alToke :: Jurado
alToke = [(criterioAabb, 0.5),(criterioCombinado, 1),(criterioPublicoExaltado, 1),(criterioPotencia, 1.5)]

-- Calcular el puntaje que le daría un jurado a una puesta en escena, considerando que es la suma de los puntajes de todos los criterios que cumpla la puesta, 
-- teniendo como máximo 3 puntos en total.
evaluaCriterio :: CriterioCompleto -> PuestaEnE -> Float
evaluaCriterio criterio puesta | (fst criterio) puesta = snd criterio
                               | otherwise = 0

calcularPuntaje :: Jurado -> PuestaEnE -> Float
calcularPuntaje jurado puesta = (sum.(map (`evaluaCriterio` puesta))) jurado

type Batalla = [PuestaEnE]


