data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: [(Nave->Nave)]
}
-- FUNCIONES NAVES --
pTurbo :: Nave -> Nave
pTurbo nave = nave {ataque = ataque nave +25}

pReparacion :: Nave -> Nave
pReparacion nave = nave {durabilidad = durabilidad nave +50, ataque = ataque nave -30}

pSuperTurbo :: Nave -> Nave
pSuperTurbo nave = pTurbo . pTurbo . pTurbo(nave {durabilidad = durabilidad nave -45})

pReparacionFalcon :: Nave -> Nave
pReparacionFalcon nave = nave {durabilidad = durabilidad nave +50, ataque = ataque nave -30, escudo = escudo nave +100}

-- FUNCIONES NAVES --





ejTIE = "TIE Fighter" 200 100 50 [pTurbo]
ejXWing = "X Wing" 300 150 100 [pReparacion]
ejNaveDarthVader = "Nave de Darth Vader" 500 300 200 [pSuperTurbo]
ejMillenniomFalcon = "Millennium Falcon" 1000 500 50 [pReparacionFalcon]