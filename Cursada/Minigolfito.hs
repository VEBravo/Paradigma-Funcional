checkNegativo :: Int -> Int
checkNegativo = max 0

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.

paloPutter :: Habilidad -> Tiro
paloPutter h = UnTiro {velocidad = 10, precision = precisionJugador h * 2, altura = 0}
  
paloMadera :: Habilidad -> Tiro
paloMadera h = UnTiro {velocidad = 100, precision = div (precisionJugador h) 2, altura = 5}

paloHierro :: Habilidad -> Int -> Tiro
paloHierro h n = UnTiro {velocidad = fuerzaJugador h*n, precision = div (precisionJugador h) n, altura = checkNegativo (n-3)}

-- Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

palos :: [Habilidad -> Tiro]
palos = [paloPutter, paloMadera] ++ map (flip paloHierro) [1..10]

golpe :: Jugador -> (Habilidad -> Tiro) -> Tiro
golpe persona palo = palo (habilidad persona)

-- Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
-- En principio necesitamos representar los siguientes obstáculos:

-- Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
-- Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

tunelConRampita :: Tiro -> (Tiro, Bool)
tunelConRampita t | precision t > 90 && altura t == 0 = (t {velocidad = velocidad t * 2, precision = 100, altura = 0}, True)
                  | otherwise = (t {velocidad = 0, precision = 0, altura =0}, False)

-- Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y 
-- precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.

laguna :: Int -> Tiro -> (Tiro, Bool)
laguna largo t | velocidad t > 80 && altura t > 1 && altura t < 5 = (t {velocidad = velocidad t, precision = precision t, altura = div (altura t) largo}, True)
               | otherwise = (t {velocidad = 0, precision = 0, altura =0}, False)

-- Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
-- Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

hoyo :: Tiro -> Int -> (Tiro, Bool)
hoyo t largo | velocidad t > 5 && velocidad t < 20 && altura t == 0 && precision t > 95 = (t {velocidad = 0, precision = 0, altura =0}, True)
             | otherwise = (t {velocidad = 0, precision = 0, altura =0}, False)
-- Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

-- Se asume que si pasas laguna pasas laguna 50

palosUtiles :: Jugador -> (Tiro -> (Tiro, Bool)) -> [Habilidad -> Tiro]
palosUtiles persona obstaculo = filter (snd.obstaculo.golpe persona) palos
-- 
