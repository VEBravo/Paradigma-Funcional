import Control.Concurrent.STM (check)
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

esAtacada :: Nave -> Nave -> Nave
esAtacada nAtacada nAtacante | escudo (poder nAtacada nAtacada) > ataque (poder nAtacante nAtacante) = nAtacada
                             | otherwise = (poder nAtacada nAtacada) {durabilidad = checkNegativo (durabilidad nAtacada - abs(ataque (poder nAtacante nAtacante) - escudo nAtacada))}

