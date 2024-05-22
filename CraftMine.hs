data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
} deriving Show


type Material = String

data Receta = UnaReceta {
    resultado :: Material,
    materiales :: [Material],
    tiempo :: Int
} deriving Show

data Bioma = UnBioma {
    materialNecesario :: Material,
    materialesBioma :: [Material]
} deriving Show



--1
cuentaConTodosLosMateriales :: Personaje -> Receta -> Bool
cuentaConTodosLosMateriales jugador receta = all (\m -> elem m (inventario jugador)) (materiales receta)

craftear :: Personaje -> Receta -> Personaje
craftear jugador receta | cuentaConTodosLosMateriales jugador receta = quitaMaterialesSegunReceta (jugador {puntaje = puntaje jugador + 10*tiempo receta, inventario = inventario jugador ++ [resultado receta]}) receta
                        | otherwise = jugador {puntaje = puntaje jugador - 100}

quitaMaterialesSegunReceta :: Personaje -> Receta -> Personaje
quitaMaterialesSegunReceta jugador receta = jugador {inventario = foldl filtrarMaterial (inventario jugador) (materiales receta)}

filtrarMaterial :: [Material] -> Material -> [Material]
filtrarMaterial inventario material = takeWhile (material/=) inventario ++ drop (length (takeWhile (material/=) inventario)+1) inventario

-- 2
crafteablesYQueDuplican :: Personaje -> [Receta] -> [Receta]
crafteablesYQueDuplican jugador recetas = filter (duplicaPuntaje jugador) (encontrarObjetosCrafteables jugador recetas)

duplicaPuntaje :: Personaje -> Receta -> Bool
duplicaPuntaje jugador receta = tiempo receta*10 + puntaje jugador >= puntaje jugador*2

encontrarObjetosCrafteables :: Personaje -> [Receta] -> [Receta]
encontrarObjetosCrafteables jugador = filter (cuentaConTodosLosMateriales jugador)

crafteoSucesivo :: Personaje -> [Receta] -> Personaje
crafteoSucesivo = foldl craftear

logroMasPuntos :: Personaje -> [Receta] -> Bool
logroMasPuntos jugador recetas= puntaje (crafteoSucesivo jugador recetas) > puntaje jugador

-- 3
cuentaConElemento :: Personaje -> Bioma -> Bool
cuentaConElemento jugador bioma = elem (materialNecesario bioma) (inventario jugador)

type Herramienta = [Material] -> Material
hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: [Material] -> Int -> Material
pico = (!!)

machete :: Herramienta
machete materiales = flip.(!!).(flip.(div 2).length) materiales
-- Toma el 


inventarioDespuesDeBioma :: Personaje -> Bioma -> Herramienta -> [Material]
inventarioDespuesDeBioma jugador bioma herramienta = inventario jugador ++ [herramienta (materialesBioma bioma)]

minar :: Personaje -> Bioma -> Herramienta -> Personaje
minar jugador bioma herramienta | cuentaConElemento jugador bioma = jugador {inventario = inventarioDespuesDeBioma jugador bioma herramienta, puntaje = puntaje jugador +50}
                                | otherwise = jugador



ejJugador1 :: Personaje
ejJugador1 = UnPersonaje "LucioBot" 500 ["Madera","Carbon","Fosforo","Fosforo"]
ejJugador2 :: Personaje
ejJugador2 = UnPersonaje "BrunoBot" 300 ["Carbon","Fosforo","Fosforo"]

ejFogata :: Receta
ejFogata = UnaReceta "Fogata" ["Madera","Fosforo"] 10
ejFogata2 :: Receta
ejFogata2 = UnaReceta "Fogata" ["Madera","Fosforo"] 10
ejPolloAsado :: Receta
ejPolloAsado = UnaReceta "PolloAsado" ["Fogata","Pollo"] 300
ejSueter :: Receta
ejSueter = UnaReceta "Sueter" ["Lana","Agujas","Tintura"] 600

ejBiomaArtico :: Bioma
ejBiomaArtico = UnBioma "Sueter" ["Hielo","Iglu","Lobo"]

