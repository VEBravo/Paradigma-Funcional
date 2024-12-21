-- TIPOS
type Hechizo = Postre -> Postre

-- MODELADO
data Postre = UnPostre {
    sabores :: [String],
    peso :: Float,
    temperatura :: Float
}deriving (Show,Eq)

data Mago = UnMago {
    hechizos :: [Hechizo],
    horrorcruxes :: Float
}
-- HECHIZOS
incendio :: Hechizo
incendio = modificarPeso (*0.95).modificarTemp (+1)

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado". modificarPeso (*0.9)

inmobulus :: Hechizo
inmobulus = modificarTemp (*0)

diffindo :: Float -> Hechizo
diffindo porcentaje = modificarPeso (*(porcentaje/100))

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra postre = inmobulus postre {sabores = []}

losDejaListos :: Hechizo -> [Postre] -> Bool
losDejaListos hechizo = all (\p -> (estaListo.hechizo) p)

-- PRO
pesoPromedioPostresListos :: [Postre] -> Float      -- $ Operador de precedencia, primero se evalua lo de la derecha, para evitar parentesis
pesoPromedioPostresListos postres = (sum . map peso $ postresListos) / (fromIntegral . length $ postresListos)
  where postresListos = filter estaListo postres

-- MAGOS
asistirClase :: Mago -> Hechizo -> Postre -> Mago
asistirClase mago hechizo postre | mismoResultado hechizo avadaKedavra postre = (agregarHechizo hechizo.modificarHorrocrux (+1)) mago
                                 | otherwise = agregarHechizo hechizo mago

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = elMejorHechizo (hechizos mago) postre

-- PRO
elMejorHechizo :: [Hechizo] -> Postre -> Hechizo
elMejorHechizo (x:[]) _ = x
elMejorHechizo (x:y:ys) postre | (length.sabores.x) postre > (length.sabores.y) postre = elMejorHechizo (x:ys) postre
                               | otherwise = elMejorHechizo (y:ys) postre

infinitosPostres = repeat tarta
messi = UnMago (repeat incendio) 2



-- AUX
modificarPeso :: (Float -> Float) -> Postre -> Postre
modificarPeso f postre = postre {peso = f (peso postre)}

modificarTemp :: (Float -> Float) -> Postre -> Postre
modificarTemp f postre = postre {temperatura = f (temperatura postre)}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabores postre ++ [sabor]}

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && (not.null.sabores) postre && temperatura postre > 0

mismoResultado :: Hechizo -> Hechizo -> Postre -> Bool
mismoResultado h1 h2 postre = h1 postre == h2 postre

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago {hechizos = hechizos mago ++ [hechizo]}

modificarHorrocrux :: (Float -> Float) -> Mago -> Mago
modificarHorrocrux modificador mago = mago {horrorcruxes = modificador (horrorcruxes mago)}
-- EJEMPLOS
bizcochoBorracho = UnPostre ["fruta","crema"] 100 25
tarta = UnPostre ["melaza"] 50 0

{- RTA: Tomo como que la funcion recibiria una lista de hechizos y una lista de postres. En este caso, lazy evaluation nos ayudaria solo
    si es falso ya que para saber si todos quedan listos, deberia utilizar un any para la lista de hechizos y un all para la lista de postres
    entonces tendria que leer la lista entera de postres, si es verdadero que ese hechizo los deja listos (algo imposible si es infinita). 
    Por otro lado, si todos los hechizos devuelven por lo menos un postre que no este listo,  entonces devolveria False. Ya que todos los 
    all devolverian False. Y el any tambien daria False
-}

{-
    C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

    RTA: En este caso, lazy evaluation no nos ayudaria ya que para saber el mejor hechizo debe leer la lista entera hasta que quede uno solo. Por lo que
    si es infinita, por mas que vayas sacando hechizos, nunca va a quedar uno solo. No hay ningun caso en el que se pueda encontrar el mejor hechizo.-}