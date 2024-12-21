import Data.List (sort)
vocales = "aeiouAEIOUÁÉÍÓÚáéíóú"

data Ciudad = UnaCiudad {
    nombre :: String,
    anioFundacion :: Float,
    atracciones :: [String],
    costoVida :: Float
} deriving (Eq, Show)

valorCiudad :: Ciudad -> Float
valorCiudad c | a < 1800 = 5 * (1800 - a)
              | null (atracciones c) = 2 * cv
              | otherwise = 3 * cv where cv = costoVida c; a = anioFundacion c

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada = any empiezaConVocal.atracciones

empiezaConVocal :: String -> Bool
empiezaConVocal = flip elem vocales . head

esSobria :: Ciudad -> Int -> Bool
esSobria ciudad n = all (masDeNLetras n) (atracciones ciudad) && (not.null.atracciones) ciudad

masDeNLetras :: Int -> String -> Bool
masDeNLetras n = (>n).length

nombreRaro :: Ciudad -> Bool
nombreRaro = (<5) . length . nombre

agregarAtraccion :: String -> Ciudad -> Ciudad
agregarAtraccion atraccion ciudad = ciudad {atracciones = (atracciones ciudad) ++ [atraccion], costoVida = (costoVida ciudad)*1.2}

crisis :: Ciudad -> Ciudad
crisis ciudad   | (null.atracciones) ciudad = ciudad {costoVida = (costoVida ciudad)*0.9}
                | otherwise = ciudad {atracciones = init (atracciones ciudad), costoVida = (costoVida ciudad)*0.9}

remodelacion :: Float -> Ciudad -> Ciudad
remodelacion porcentaje ciudad = ciudad {nombre = "New " ++ (nombre ciudad), costoVida = costoVida ciudad * (porcentaje/100 + 1)}

reevaluacion :: Int -> Ciudad -> Ciudad
reevaluacion n ciudad | esSobria ciudad n = ciudad {costoVida = (costoVida ciudad)*1.1}
                      | otherwise = ciudad {costoVida = (costoVida ciudad) - 3}

laTransformacionNoPara :: Float -> Int -> Ciudad -> Ciudad
laTransformacionNoPara porcentaje letras = reevaluacion letras . crisis . remodelacion porcentaje

data Anio = UnAnio {
    numero :: Int,
    eventos :: [(Ciudad -> Ciudad)]
}

reflejarPasoAnio :: Anio -> Ciudad -> Ciudad
reflejarPasoAnio anio ciudad = foldl (\ciudad funcion -> funcion ciudad) ciudad (eventos anio)

algoMejor :: Ord a => Ciudad -> (Ciudad -> a) -> (Ciudad -> Ciudad) -> Bool
algoMejor ciudad criterio evento = criterio ciudad < (criterio.evento) ciudad

soloCostoVidaQueSuba :: Anio -> Ciudad -> Ciudad
soloCostoVidaQueSuba anio ciudad = reflejarPasoAnio (UnAnio (numero anio) (filter (algoMejor ciudad costoVida) (eventos anio))) ciudad

soloCostoVidaQueBaje :: Anio -> Ciudad -> Ciudad
soloCostoVidaQueBaje anio ciudad = reflejarPasoAnio (UnAnio (numero anio) (filter (not.algoMejor ciudad costoVida) (eventos anio))) ciudad

valorQueSuba :: Anio -> Ciudad -> Ciudad
valorQueSuba anio ciudad = reflejarPasoAnio (UnAnio (numero anio) (filter (algoMejor ciudad valorCiudad) (eventos anio))) ciudad

eventosOrdenados :: Anio -> Ciudad -> Bool
eventosOrdenados anio ciudad = map (\evento -> (costoVida.evento) ciudad) (eventos anio) == sort (map (\evento -> (costoVida.evento) ciudad) (eventos anio))

ciudadOrdenadas :: (Ciudad -> Ciudad) -> [Ciudad] -> Bool
ciudadOrdenadas _ [] = True
ciudadOrdenadas evento [_] = True
ciudadOrdenadas evento (x:y:xs) = (costoVida . evento) x < (costoVida . evento) y && ciudadOrdenadas evento (y:xs)

aniosOrdenados :: [Anio] -> Ciudad -> Bool
aniosOrdenados anios ciudad = all (flip eventosOrdenados ciudad) anios && map costoVida (map (flip reflejarPasoAnio ciudad) anios) == sort (map costoVida (map (flip reflejarPasoAnio ciudad) anios))


-- EJEMPLOS
baradero = UnaCiudad "Baradero" 1615 ["Parque del este","Museo Alejandro Barbich"] 150
nullish = UnaCiudad "Nullish" 1800 [] 140
caletaOliva = UnaCiudad "Caleta Oliva" 1901 ["El Gorosito","Faro Costanera"] 120
maipu = UnaCiudad "Maipu" 1878 ["Fortin Kakel"] 115
azul = UnaCiudad "Azul" 1832 ["Teatro Espanol","Parque Municipal Sarmiento","Costanera Cacique Catriel"] 190

ej2022 = UnAnio 2022 [crisis, remodelacion 5, reevaluacion 7]
ej2021 = UnAnio 2021 [crisis, agregarAtraccion "playa"]
ej2023 = UnAnio 2023 [crisis, agregarAtraccion "parque", remodelacion 10, remodelacion 20]
ej2015 = UnAnio 2015 []