--------------- TIPOS ------------------
type Material = String
type Herramienta = [Material] -> Material

--------------- MODELADO ------------------
data Receta = UnaReceta {
    resultado :: Material,
    tiempo :: Float,
    materiales :: [Material]
}

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Float,
    inventario :: [Material]
}deriving (Show)

data Bioma = UnBioma {
    recursos :: [Material],
    requisito :: Material
}deriving (Show)
--------------- FUNCIONES CRAFT ------------------
craftearObjeto :: Receta -> Personaje -> Personaje
craftearObjeto receta personaje | puedeHacer personaje receta = ((modificarPuntaje (+10*(tiempo receta))).(agregarMaterial (resultado receta)).(utilizarMateriales (materiales receta))) personaje
                                | otherwise = modificarPuntaje (\p->p-100) personaje

objetosConvenientes :: Personaje -> [Receta] -> [Receta]
objetosConvenientes personaje = filter (\r -> puedeHacer personaje r && (puntajePostReceta r personaje)/2 == puntaje personaje)

craftearSucesivamente :: [Receta] -> Personaje -> Personaje
craftearSucesivamente receta personaje = foldl (\p r -> craftearObjeto r p) personaje receta

ganaMasPuntosAlReves :: Personaje -> [Receta] -> Bool
ganaMasPuntosAlReves personaje recetas = (puntaje.craftearSucesivamente (reverse recetas)) personaje > (puntaje.craftearSucesivamente recetas) personaje

--------------- FUNCIONES MINE ------------------
irAMinar :: Bioma -> Herramienta -> Personaje -> Personaje
irAMinar bioma herramienta personaje | tieneMaterial personaje (requisito bioma) = ((modificarPuntaje (+50)).agregarMaterial (herramienta (recursos bioma))) personaje
                                     | otherwise = personaje

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Int -> Herramienta
pico indice materiales = materiales !! indice

pala :: Herramienta
pala materiales = materiales !! (((`div` 2).length) materiales)

taladro :: Herramienta
taladro = find (\m -> elem 'a' m)
--------------- AUXILIARES ---------------
modificarPuntaje :: (Float -> Float) -> Personaje -> Personaje
modificarPuntaje f personaje = personaje {puntaje = f (puntaje personaje)}

tieneMaterial :: Personaje -> Material -> Bool
tieneMaterial personaje material = any (\m -> m == material) (inventario personaje)

puedeHacer :: Personaje -> Receta -> Bool
puedeHacer personaje receta = all (\m -> tieneMaterial personaje m) (materiales receta)

utilizarMateriales :: [Material] -> Personaje -> Personaje
utilizarMateriales receta personaje = personaje {inventario = actualizarInventario receta (inventario personaje)}

actualizarInventario :: [Material] -> [Material] -> [Material]
actualizarInventario [] inventario = inventario 
actualizarInventario receta (x:xs) | elem x receta = actualizarInventario (quitarElementoLista x receta) xs
                                   | otherwise = x : actualizarInventario receta xs

quitarElementoLista :: Material -> [Material] -> [Material]
quitarElementoLista e = filter (\item -> e /= item)

agregarMaterial :: Material -> Personaje -> Personaje
agregarMaterial material personaje = personaje {inventario = inventario personaje ++ [material]}

puntajePostReceta :: Receta -> Personaje -> Float
puntajePostReceta receta = (puntaje.(craftearObjeto receta))

--------------- PERSONAJES ---------------
pedro = UnPersonaje "Pedro" 1500 ["fosforo"]
juan = UnPersonaje "Juan" 1000 ["sueter","fogata","pollo","pollo"]


--------------- RECETAS ---------------
fogata = UnaReceta "fogata" 10 ["madera","fosforo"]
polloAsado = UnaReceta "polloAsado" 300 ["fogata","pollo"]
sueter = UnaReceta "sueter" 600 ["lana","agujas","tintura"]

--------------- BIOMAS ---------------
artico = UnBioma ["hielo","iglu","lobo"] "sueter"

