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
    fuerza :: Float
} deriving (Show)

pedrito = Nomus {
    alas = True,
    cantBrazos = 10,
    cantOjos = 1,
    colorDePiel = "Verde",
    cantVida = 500,
    fuerza = 7000
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


