checkNegativo :: Int -> Int
checkNegativo = max 0

------------------------- Estructuras y definicion de datos -------------------------

data Turista = UnTurista {
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving (Eq, Show)

type Idioma = String
type ElementoPaisaje = String
type Minutos = Int
type Marea = String
type Excursion = (Turista -> Turista)
type Indice = (Turista -> Int)
type Tour = [Excursion]

-------------------------------------------------------------------------------------

-- Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Turista -> Turista
irALaPlaya turista | viajaSolo turista = turista {cansancio= cansancio turista - 5}
                   | otherwise = turista {cansancio= cansancio turista - 1}

-- Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia
apreciarElemento ::  ElementoPaisaje -> Turista -> Turista 
apreciarElemento elemento turista = turista {cansancio = (checkNegativo.(stress turista -).length) elemento}

-- Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
salirHablarIdioma ::  Idioma -> Turista ->Turista
salirHablarIdioma idioma turista = turista {viajaSolo = False, idiomas = idiomas turista ++ [idioma]}

-- Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. 
-- El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
nivelIntensidad :: Minutos -> Int
nivelIntensidad minutos = div minutos 4

caminarCiertosMinutos :: Minutos -> Turista -> Turista
caminarCiertosMinutos minutos turista = turista {cansancio = cansancio turista + nivelIntensidad minutos, stress = cansancio turista - nivelIntensidad minutos}

-- Paseo en barco: depende de cómo esté la marea
    -- si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
    -- si está moderada, no pasa nada.
    -- si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.

paseoEnBarco :: Marea -> Turista -> Turista
paseoEnBarco marea turista | marea == "Fuerte"    = turista {stress = stress turista + 6, cansancio = cansancio turista + 10}
                           | marea == "Moderada"  = turista
                           | marea == "Tranquila" = ((caminarCiertosMinutos 10).(apreciarElemento "mar").(salirHablarIdioma "Aleman")) turista

-- Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones 
-- existentes. Además:
-- Hacer que un turista haga una excursión. 
-- Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.

hacerExcursion :: Turista -> Excursion -> Turista
hacerExcursion turista excursion = excursion (turista {stress = checkNegativo (stress turista - 1)})

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el 
-- turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = abs (indice turista - indice (excursion turista))

-- Usar la función anterior para resolver cada uno de estos puntos:
-- Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = (length.idiomas) (excursion turista) > (length.idiomas) turista

-- Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = deltaExcursionSegun stress turista excursion > 2

-- Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones
-- Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", 
-- luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl hacerExcursion turista tour

ejCompleto :: Tour
ejCompleto = [caminarCiertosMinutos 20, apreciarElemento "cascada", caminarCiertosMinutos 40, irALaPlaya, salirHablarIdioma "Melmacquiano"]

tourCompleto :: Turista -> Turista
tourCompleto turista = hacerTour turista ejCompleto

-- Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
-- Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y 
-- finalmente vuelve caminando hasta la otra punta, tardando 2 horas.

tourLadoB :: Turista -> Excursion -> Turista
tourLadoB turista excursion = hacerTour turista ([paseoEnBarco "Tranquila"]++[excursion]++[caminarCiertosMinutos 120])

-- Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: 
-- si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, 
-- y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.

tourIslaVecina :: Turista -> Marea -> Excursion -> Turista
tourIslaVecina turista marea excursion | marea == "Fuerte" = hacerTour turista [paseoEnBarco marea, excursion, apreciarElemento "lago", paseoEnBarco marea]
                                       | otherwise = hacerTour turista [paseoEnBarco marea, excursion, irALaPlaya, paseoEnBarco marea]

-- Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión desestresante la cual, además, 
-- deja al turista acompañado luego de realizarla.

dejaAcompañadoYDesestresante :: Turista -> Excursion -> Bool
dejaAcompañadoYDesestresante turista excursion = esDesestresante turista excursion && viajaSolo (excursion turista)

hayTourConvincente :: Turista -> [Tour] -> Bool
hayTourConvincente turista tours = any (True ==) (map (any (dejaAcompañadoYDesestresante turista)) tours)

-- Implementar y contestar en modo de comentarios o pruebas por consola
-- Construir un tour donde se visiten infinitas playas.
visitarInfPlayas :: Turista -> Turista
visitarInfPlayas turista = visitarInfPlayas (irALaPlaya turista)

-- ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
ejListaInfinita :: Tour
ejListaInfinita = repeat irALaPlaya

-- ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

-- -- Turistas Ejemplos
ejJuan :: Turista
ejJuan = UnTurista 50 20 False ["Alemán","Portugues","Español","Inglés"]

ejMariana :: Turista
ejMariana = UnTurista 10 10 True ["Español","Inglés"]

ejAna :: Turista
ejAna = UnTurista 0 21 False ["Español"]

ejBeto :: Turista
ejBeto = UnTurista 15 15 True ["Alemán"]

ejCathi :: Turista
ejCathi = UnTurista 15 15 True ["Alemán","Catalán"] 
