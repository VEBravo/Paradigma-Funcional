-- TIPOS
type Tribu = [Fremen]
type Titulo = String
type Mision = Gusano -> Fremen -> Fremen
-- MODELADO
data Fremen = UnFremen {
    nombre :: String,
    nivelTolerancia :: Float,
    titulos :: [Titulo],
    reconocimientos :: Float
} deriving(Show,Eq)

data Gusano = UnGusano {
    longitud :: Float,
    hidratacion :: Float,
    descripcion :: String
} deriving(Show)

-- FUNCIONES - FREMEN
recibirRecon :: Fremen -> Fremen
recibirRecon fremen = fremen {reconocimientos = reconocimientos fremen + 1}

hayCandidato :: Tribu -> Bool
hayCandidato = any esCandidato

elElegido :: Tribu -> Fremen
elElegido = masTitulos.filter (\f -> esCandidato f)

-- FUNCIONES - GUSANOS
reproducir :: Gusano -> Gusano -> Gusano
reproducir padre madre = UnGusano  (alturaHijo padre madre) 0 (descripcion madre ++ " - " ++ descripcion padre)

aparearListas :: [Gusano] -> [Gusano] -> [Gusano]
aparearListas [] _ = [] 
aparearListas _ [] = [] 
aparearListas (x:xs) (y:ys) = [reproducir x y] ++ aparearListas xs ys 

domarGusano :: Mision
domarGusano gusano fremen | puedeDomar fremen gusano = (conseguirTitulo "Domador".(modificarTolerancia (+100))) fremen
                          | otherwise = modificarTolerancia (*0.9) fremen

destruirGusano :: Mision
destruirGusano gusano fremen | puedeDestruir fremen gusano = (recibirRecon.(modificarTolerancia (+100))) fremen
                             | otherwise = modificarTolerancia (*0.8) fremen

misionColectiva :: Mision -> Gusano -> Tribu -> Tribu
misionColectiva mision gusano = map (\f -> mision gusano f) 

misionCambiaElegido :: Mision -> Gusano -> Tribu -> Bool
misionCambiaElegido mision gusano tribu = (nombre.elElegido) tribu /= (nombre.elElegido.misionColectiva mision gusano) tribu

-- AUXILIARES
alturaHijo :: Gusano -> Gusano -> Float
alturaHijo padre = ((*0.1).max (longitud padre).longitud)

esCandidato :: Fremen -> Bool
esCandidato fremen = tieneTitulo "Domador" fremen && ((>100).nivelTolerancia) fremen

tieneTitulo :: Titulo -> Fremen -> Bool
tieneTitulo titulo = elem titulo.titulos

masTitulos :: Tribu -> Fremen
masTitulos (x:[]) = x
masTitulos (x:y:ys) | reconocimientos x > reconocimientos y = masTitulos (x:ys)
                    | otherwise = masTitulos (y:ys)

puedeDomar :: Fremen -> Gusano -> Bool
puedeDomar fremen gusano = nivelTolerancia fremen < ((/2).longitud) gusano

conseguirTitulo :: Titulo -> Fremen -> Fremen
conseguirTitulo titulo fremen = fremen {titulos = titulos fremen ++ [titulo]}

modificarTolerancia :: (Float -> Float) -> Fremen -> Fremen
modificarTolerancia modificador fremen = fremen {nivelTolerancia = modificador (nivelTolerancia fremen)}

puedeDestruir :: Fremen -> Gusano -> Bool
puedeDestruir fremen gusano = tieneTitulo "Domador" fremen && nivelTolerancia fremen < longitud gusano/2

-- EJEMPLOS
stilgar = UnFremen "Stilgar" 150 ["Guía"] 3
franco = UnFremen "Franco" 120 ["Capo"] 5
valen = UnFremen "Valentin" 101 ["Coder","Domador"] 10

tribu = [stilgar,franco,valen]

gusanoRojo = UnGusano 10 5 "rojo con lunares"
gusanoDientes = UnGusano 8 1 "dientes puntiagudos"
