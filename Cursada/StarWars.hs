import Control.Concurrent.STM (check)
import qualified Main as fueraDeCombate
data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Nave->Nave
}

-- FUNCIONES NAVES --
checkNegativo :: Int -> Int
checkNegativo = max 0

pTurbo :: Nave -> Nave
pTurbo nave = nave {ataque = ataque nave +25}

pReparacion :: Nave -> Nave
pReparacion nave = nave {durabilidad = durabilidad nave +50, ataque = checkNegativo (ataque nave -30)}

pSuperTurbo :: Nave -> Nave
pSuperTurbo nave = (pTurbo . pTurbo . pTurbo) (nave {durabilidad = checkNegativo (durabilidad nave - 45)})

pReparacionFalcon :: Nave -> Nave
pReparacionFalcon nave = pReparacion (nave {escudo = escudo nave +100})

pTurboReparacion :: Nave -> Nave
pTurboReparacion nave = (pTurbo . pReparacion) (nave {nombre = "Super" ++ nombre nave})

ejTIE = UnaNave "TIE Fighter" 200 100 50 pTurbo
ejXWing = UnaNave "X Wing" 300 150 100 pReparacion
ejNaveDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 pSuperTurbo
ejMillenniomFalcon = UnaNave "Millennium Falcon" 1000 500 50 pReparacionFalcon

--  DURABILIDAD TOTAL DE FLOTA  --
ejFlota1 = [ejTIE, ejXWing, ejNaveDarthVader, ejMillenniomFalcon]

durabilidadFlota :: [Nave] -> Int
durabilidadFlota = sum . map durabilidad

-- Saber cómo queda una nave luego de ser atacada por otra. 
-- Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce su durabilidad según el daño recibido, 
-- que es la diferencia entre el ataque de la atacante y el escudo de la atacada. (si el escudo es superior al ataque, la nave atacada no es afectada). 
-- La durabilidad, el escudo y el ataque nunca pueden ser negativos, a lo sumo 0.


esAtacada :: Nave -> Nave -> Nave
esAtacada nAtacante nAtacada | escudo (poder nAtacada nAtacada) > ataque (poder nAtacante nAtacante) = nAtacada
                             | otherwise = (poder nAtacada nAtacada) {durabilidad = checkNegativo (durabilidad nAtacada - abs(ataque (poder nAtacante nAtacante) - escudo nAtacada))}

-- Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0. 
fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

-- Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia. 
-- Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de la flota. 
-- Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves de la flota que la estrategia determine que conviene atacar.
-- Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:

-- 1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.
misionSorpresa :: Nave -> [Nave] -> (Nave->Bool) -> [Nave]
misionSorpresa naveAtacante flota estrategia = map (esAtacada nave) (filter estrategia flota)

-- 2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. Por ejemplo, en alguna misión se podría utilizar 
-- una estrategia de peligrosidad mayor a 300, y en otra una estrategia de peligrosidad mayor a 100.
estrategiaPeligrosidad :: Int -> Nave -> Bool -- Al pasar esta estrategia se pasa como estrategiaPeligrosidad 300
estrategiaPeligrosidad valor nave = ataque nave > valor

-- 3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque de la nave atacante quedan fuera de combate. 
estrategiaFueraDeCombate :: Nave -> Nave -> Bool -- Al pasar esta estrategia se pasa como estrategiaPeligrosidad naveAtacante
estrategiaFueraDeCombate naveAtacante naveAtacada = (fueraDeCombate.esAtacada) naveAtacante naveAtacada

-- Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y 
-- llevar adelante una misión con ella.
mejorEstrategia :: (Nave -> Bool) -> (Nave -> Bool) -> Nave -> [Nave]
mejorEstrategia strat1 strat2 naveAtacante flota | durabilidadFlota (misionSorpresa naveAtacante flota strat1) > durabilidadFlota (misionSorpresa naveAtacante flota strat2) = misionSorpresa naveAtacante flota strat2
                                                 | durabilidadFlota (misionSorpresa naveAtacante flota strat1) < durabilidadFlota (misionSorpresa naveAtacante flota strat2) = misionSorpresa naveAtacante flota strat1