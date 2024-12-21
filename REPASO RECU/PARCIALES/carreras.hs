import Data.List (elemIndex,find)
import Data.Maybe (fromMaybe)

-- AUXILIARES
indice :: Eq a => [a] -> a -> Int
indice lista elemento = fromMaybe (-1) (elemIndex elemento lista)
-------------
type Carrera = [Auto]
type Color = String

data Auto = UnAuto{
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving(Eq,Show)

-- carrera -> estado de los autos
mario = UnAuto "rojo" 120 0
fantasma = UnAuto "blanco" 120 0
elNegro = UnAuto "negro" 120 0

carrera :: Carrera
carrera = [mario,fantasma,elNegro]

estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 = a1 /= a2 && abs (distancia a2 - distancia a1) < 10

vaTranquilo :: Auto -> Bool
vaTranquilo auto = all (\a -> distancia auto > distancia a && estaCerca a auto) carrera

puesto :: Auto -> Int
puesto = (1+).(indice carrera)

correr :: Int -> Auto -> Auto
correr tiempo auto = auto {distancia = distancia auto + velocidad auto * tiempo}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cuanto auto = auto {velocidad = max 0 (velocidad auto - cuanto)}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) carrera

miguelitos :: Int -> Auto -> Carrera
miguelitos cantidad auto =  afectarALosQueCumplen ((indice carrera auto<).(indice carrera)) (bajarVelocidad cantidad) carrera

jetpack :: Int -> Auto -> Carrera
jetpack tiempo auto = afectarALosQueCumplen (auto ==) (correr (tiempo*2)) carrera

podio :: Carrera -> [(Int,Color)]
podio = map (\a -> (indice carrera a, color a))

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int,Color)]
simularCarrera carrera eventos = podio (foldl (\carrera evento -> evento carrera) carrera eventos )

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo = map (correr tiempo)

usaPowerUp :: (Auto -> Carrera) -> Color -> Carrera -> Carrera
usaPowerUp powerUp elColor = powerUp.head.(filter ((elColor==).color))

eventos :: [Carrera -> Carrera]
eventos = [correnTodos 30, usaPowerUp (jetpack 3) "azul", usaPowerUp terremoto "blanco", correnTodos 40, usaPowerUp (miguelitos 20) "blanco", usaPowerUp (jetpack 6) "negro", correnTodos 10]

