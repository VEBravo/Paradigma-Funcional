checkNegativo :: Int -> Int
checkNegativo num = max 0 num

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}

data Personaje = UnPersonaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    planeta :: Planeta
} deriving (Eq, Show)

type Universo = [Personaje]
type Gema = Personaje -> Personaje
type Habilidad = String
type Planeta = String

------------------------------------------ PRIMERA PARTE ------------------------------------------------------

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | (length.gemas) guantelete == 6 && material guantelete == "uru" = take (div (length universo) 2) universo
                              | otherwise = universo

esPendex :: Personaje -> Bool
esPendex personaje = edad personaje < 45

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any esPendex universo

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad personaje = (length.habilidades) personaje > 1

energiaTotal :: Universo -> Int
energiaTotal universo = (sum.(map energia).filter (masDeUnaHabilidad)) universo

------------------------------------------ SEGUNDA PARTE ------------------------------------------------------
gemaMente :: Personaje -> Int -> Personaje
gemaMente personaje valor = personaje {energia = energia personaje - valor}

gemaAlma :: Personaje -> Habilidad -> Personaje
gemaAlma personaje habilidad | elem habilidad (habilidades personaje) =  gemaMente (personaje {habilidades = filter (habilidad /=) (habilidades personaje)}) 10
                             | otherwise = personaje

gemaEspacio :: Personaje -> Planeta -> Personaje
gemaEspacio personaje planeta = gemaMente (personaje {planeta = planeta}) 20

gemaPoder :: Personaje -> Personaje
gemaPoder personaje | (length.habilidades) personaje <= 2 = gemaMente (personaje {habilidades = []}) (energia personaje)
                    | otherwise = personaje {energia = 0}

gemaTiempo :: Personaje -> Personaje
gemaTiempo personaje = personaje {edad = (max 18) (div (edad personaje) 2)}

ejecutarGema :: Personaje -> Gema -> Personaje
ejecutarGema personaje gema = gema personaje

gemaLoca :: Personaje -> Gema -> Personaje
gemaLoca personaje gema = foldl ejecutarGema personaje [gema, gema]

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = UnGuantelete "goma" [gemaTiempo, flip gemaAlma "usar Mjolnir", (flip gemaLoca (flip gemaAlma "programacion en Haskell"))]

utilizarGemas :: [Gema] -> Personaje -> Personaje
utilizarGemas gemas personaje = foldl ejecutarGema personaje gemas

gemaMasPoderosa :: Guantelete -> Personaje -> (Personaje -> Personaje)
gemaMasPoderosa guante = conseguirGemaMasPoderosa (gemas guante)

conseguirGemaMasPoderosa :: [Personaje -> Personaje] -> Personaje -> (Personaje -> Personaje)
conseguirGemaMasPoderosa [] _ = id
conseguirGemaMasPoderosa [x] _ = x
conseguirGemaMasPoderosa (x:y:ys) personaje
    | (energia.x) personaje > (energia.y) personaje = conseguirGemaMasPoderosa (x:ys) personaje
    | otherwise = conseguirGemaMasPoderosa (y:ys) personaje

-- Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas gemaTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizarGemas . take 3. gemas) guantelete

-- (1) No se podría ejecutar gemaMasPoderosa punisher guanteletesDeLocos porque guanteleteDeLocos tiene infinitas gemas, por lo que el resultado de evaluar cual es la gema más poderosa es divergente
--     esto se debe a que nunca podría terminar de evaluar todas las gemas, debido a la longitud infinita de la lista.|

-- (2) Podría utilizar y me quedaría lo siguiente

-- usoLasTresPrimerasGemas guanteleteDeLocos ironMan 
-- UnPersonaje {nombre = "Tony Stark", edad = 18, energia = 300, habilidades = ["Volar","Rayo l\225ser"], planeta = "Tierra"}

-- Esto se debe a que Haskell utiliza la evaluación diferida, tiene un algoritmo vago, por lo que a diferencia de otros lenguajes, para aplicar las primeras tres gemas no recorre toda la lista,
-- va recorriendo la lista a medida que lo necesita y una vez que evaluó la condición que necesitaba devuelve el resultado, sin importarle lo que quedó de la lista sin evaluar.

------------------------------------------ EJEMPLOS ------------------------------------------------------

ironMan :: Personaje
ironMan = UnPersonaje "Tony Stark" 45 300 ["Volar","Rayo láser"] "Tierra"

capitanAmerica :: Personaje
capitanAmerica = UnPersonaje "Steve Rogers" 200 200 ["Resitencia","Arrojar Escudo"] "Tierra"

drStrange :: Personaje
drStrange = UnPersonaje "Dr Extraño" 50 800 ["Volar","Telequinesis"] "Tierra"

universo1 :: Universo
universo1 = [ironMan, capitanAmerica, drStrange, groot, wolverine]

groot :: Personaje
groot = UnPersonaje "Groot" 30 200 ["hablar", "golpear"] "Tierra"

wolverine :: Personaje
wolverine = UnPersonaje "Logan" 100 80 ["regeneración", "sentidos aumentados"] "Tierra"