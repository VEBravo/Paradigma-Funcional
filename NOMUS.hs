-- <!-- Los Nomus son humanos mutados que poseen distintas capacidades físicas, como
-- tener alas, múltiples brazos, cantidad de ojos y el color de piel, además de tener una
-- cantidad de vida y fuerza.

-- Luego se nos pide averiguar si puede ver, es decir, si tiene ojos y su categoría.

-- Categorías de un Nomu:
-- - Los nomus comunes son aquellos que su fuerza es inferior a 3000 y mayor a
-- 1000
-- - Los nomus fuertes son aquellos que su fuerza es inferior a 10000 y mayor que
-- 3000
-- - Por último, los nomus high-end poseen una fuerza mayor a 10000
-- - En otro caso son categoría “pichi” -->

data Nomus = Nomus {
    alas :: Bool,
    cantBrazos :: Int,
    cantOjos :: Int,
    colorDePiel :: String,
    cantVida :: Float,
    fuerza :: Float,
    poder :: [Poder]
} deriving (Show)

data Poder = Poder {
    cantCuracion :: Int,
    cantDanio :: Int,
    rango :: Int,
    probabilidadDanioCritico :: Float
} deriving (Show)
pedrito = Nomus {
    alas = True,
    cantBrazos = 10,
    cantOjos = 1,
    colorDePiel = "Verde",
    cantVida = 500,
    fuerza = 7000,
    poder = [superFuerza, fuego]
}
fuego = Poder {
    cantCuracion = 0,
    cantDanio = 0,
    rango = 200,
    probabilidadDanioCritico = 1.5
}
superFuerza = Poder {
    cantCuracion = 2,
    cantDanio = 10,
    rango = 4,
    probabilidadDanioCritico = 0.5
}
regeneracion = Poder {
    cantCuracion = 10,
    cantDanio = 0,
    rango = 20,
    probabilidadDanioCritico = 0
}
-- Diferencia entre el tipo y el constructor
-- Qué diferencia hay entre pasarle Nomus acá abajo y pasarle (unNomus a b o p v f) → Sirve para abrir paquete y hacer pattern matching

asignaCategoria :: Nomus -> String
asignaCategoria nomu | f>10000 = "HighEnd"
               | f>3000 = "Fuerte"
               | f>1000 = "Comun"
               | otherwise = "Pichi"
               where f = (fuerza nomu)

puedeVer :: Nomus -> Bool
puedeVer nomu | (cantOjos nomu)>0 = True
              | (cantOjos nomu)==0 = False

-- Otra cosa que no contemplamos es que los Nomus pueden tener muchos poderes,
-- como super regeneración, super fuerza, fuego, y teletransportación, entre otros…
-- Sabemos que un poder tiene:
-- - Cantidad de curación por uso
-- - Cantidad de daño por uso
-- - Rango de ataque
-- - Probabilidad de daño crítico

-- Se pide:
-- 1) Averiguar la probabilidad de daño crítico del último poder que un Nomu
-- consiguió.
-- 2) Saber si un poder es usado cuerpo a cuerpo, esto está definido por su rango
-- de ataque, siendo cuerpo a cuerpo si el rango es menor a 100.
-- 3) Saber si un poder es solamente de curación(esto pasa cuando no tiene
-- cantidad de daño por uso y si tiene curación por uso)

devuelveProbabilidadUltimoPoder :: Nomus -> Float
devuelveProbabilidadUltimoPoder (Nomus _ _ _ _ _ _ poder) = probabilidadDanioCritico (last poder)

esCuerpoACuerpo :: Poder -> Bool
esCuerpoACuerpo poder = rango (poder) < 100

esSoloCuracion :: Poder -> Bool
esSoloCuracion poder = cantDanio (poder) == 0 && cantCuracion (poder) /= 0
