-- TIPOS
type Nombre = String
type Puntos = Float
type Palo = Habilidad -> Tiro

-- MODELADO
data Habilidad = UnaHabilidad {
    fuerza :: Float,
    precisionJugador :: Float
} deriving (Eq,Show)

data Jugador = UnJugador {
    nombre :: Nombre,
    padre :: Nombre,
    habilidad :: Habilidad
} deriving (Eq,Show)

data Tiro = UnTiro {
  velocidad :: Float,
  precision :: Float,
  altura :: Float
} deriving (Eq, Show)

data Obstaculo = UnObstaculo {
    tiroPost :: Tiro -> Tiro,
    puedeSuperar :: Tiro -> Bool
}

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

indice :: Eq a => a -> [a] -> Int
indice elemento lista = fromMaybe (-1) (elemIndex elemento lista)

paloPutter :: Habilidad -> Tiro
paloPutter habilidad = UnTiro 10 (2*(precisionJugador habilidad)) 0

paloMadera :: Habilidad -> Tiro
paloMadera habilidad = UnTiro 100 ((precisionJugador habilidad)/2) 5

paloHierro :: Float -> Habilidad -> Tiro
paloHierro n habilidad = UnTiro ((fuerza habilidad)*n) ((precisionJugador habilidad)/n) (max 0 (n-3))

palos :: [Palo]
palos = [paloPutter, paloMadera, paloHierro 1, paloHierro 2, paloHierro 3, paloHierro 4, paloHierro 5, paloHierro 6, paloHierro 7, paloHierro 8, paloHierro 9, paloHierro 10]  

golpe :: Palo -> Jugador -> Tiro
golpe palo = (palo.habilidad)

-- OBSTACULOS
tiroDetenido = UnTiro 0 0 0

puedeSuperarTunel :: Tiro -> Bool
puedeSuperarTunel tiro = ((>90).precision) tiro && ((==0).altura) tiro

postTunel :: Tiro -> Tiro
postTunel tiro | puedeSuperarTunel tiro = tiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
               | otherwise = tiroDetenido

tunel = UnObstaculo postTunel puedeSuperarTunel

puedeSuperarLaguna :: Tiro -> Bool
puedeSuperarLaguna tiro = ((>80).velocidad) tiro && ((between 1 5 (altura tiro)))

postLaguna :: Float -> Tiro -> Tiro
postLaguna n tiro | puedeSuperarLaguna tiro = tiro {altura = (altura tiro) / n}
                            | otherwise = tiroDetenido

laguna n = UnObstaculo (postLaguna n) puedeSuperarLaguna

puedeSuperarHoyo :: Tiro -> Bool
puedeSuperarHoyo tiro = between 5 20 (velocidad tiro) && ((==0).altura) tiro && ((>95).precision) tiro

postHoyo :: Tiro -> Tiro
postHoyo _ = tiroDetenido

hoyo = UnObstaculo postHoyo puedeSuperarHoyo

-- 4
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (\palo -> puedeSuperar obstaculo (golpe palo jugador)) palos

cuantosPuedeSuperar :: Tiro -> [Obstaculo] -> Int
cuantosPuedeSuperar _ [] = 0
cuantosPuedeSuperar tiro (x:xs) | puedeSuperar x tiro = 1 + cuantosPuedeSuperar (tiroPost x tiro) xs
                                | otherwise = 0

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = palos !! indice (maximumPalos palos) palos
  where
    maximoSuperados = maximum (map (\p -> cuantosPuedeSuperar (golpe p jugador) obstaculos) palos)
    maximumPalos = filter (\p -> cuantosPuedeSuperar (golpe p jugador) obstaculos == maximoSuperados)

padresPerdedores :: [(Jugador,Puntos)] -> [Nombre]
padresPerdedores (x:xs) = 3

puntajeMaximo :: [(Jugador,Puntos)] -> Int
puntajeMaximo jugadores = maximum (map snd jugadores)