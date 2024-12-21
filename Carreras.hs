import Text.XHtml (alt)
-- EXTRAS
checkNegativo :: Int -> Int
checkNegativo = max 0

-- De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), 
-- la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

-- De la carrera sólo nos interesa el estado actual de los autos que están participando, lo cual nos permitirá analizar cómo viene cada uno,
--  y posteriormente procesar aquellos eventos que se den en la carrera para determinar el resultado de la misma.

-- Teniendo en cuenta lo descrito anteriormente se pide resolver los siguientes puntos explicitando el tipo de cada función desarrollada y 
-- utilizando los conceptos aprendidos del Paradigma Funcional, poniendo especial énfasis en el uso de Composición, Aplicación Parcial y Orden Superior.

-- Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y definir funciones para resolver los siguientes problemas:

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Eq, Show)


type Carrera = [Auto] -- ej [a1, a2, a3] Donde a1 es el primero en la carrera

-- Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 = abs (distanciaRecorrida a1 - distanciaRecorrida a2) < 10

-- Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto (primero:resto) = not (any (estaCerca auto) resto) && primero == auto

-- Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
puesto :: Auto -> Carrera -> Int
puesto auto carrera = length (takeWhile (/=auto) carrera) + 1

-- Desarrollar las funciones necesarias para manipular el estado de los autos para que sea posible:
-- Hacer que un auto corra durante un determinado tiempo. 
-- Luego de correr la cantidad de tiempo indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.
correr :: Auto -> Int -> Auto
correr auto tiempo = auto {distanciaRecorrida= distanciaRecorrida auto + tiempo*velocidad auto}

-- A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.
alterarVelocidad :: Auto -> (Int -> Int) -> Auto
alterarVelocidad auto modificador = auto {velocidad= modificador (velocidad auto)}

-- Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, 
-- y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.
bajarVelocidad :: Int -> Int -> Int
bajarVelocidad cantidad velocidadActual= checkNegativo (velocidadActual - cantidad)

-- Como se explicó inicialmente sobre las carreras que queremos simular, los autos que participan pueden gatillar poderes especiales a los que denominamos power ups.
-- Estos poderes son variados y tienen como objetivo impactar al estado general de la carrera, ya sea afectando al auto que lo gatilló y/o a sus contrincantes dependiendo de qué poder se trate.

-- Nota: disponemos de una función afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a] que puede ser de utilidad para manipular el estado de la carrera. Ver pág. 2 para más detalles.

-- terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
bajaSiCerca :: Auto -> Auto -> Auto
bajaSiCerca gatillo auto | estaCerca gatillo auto = alterarVelocidad auto (bajarVelocidad 50)
                         | otherwise = auto

poderTerremoto :: Auto -> Carrera -> Carrera
poderTerremoto gatillo = map (bajaSiCerca gatillo)

-- miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. 
-- Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
poderMiguelitos :: Auto -> Carrera -> Int -> Carrera
poderMiguelitos gatillo carrera cantidad = take (puesto auto carrera) carrera ++ map ((alterarVelocidad auto) (bajarVelocidad cantidad)) (drop (puesto auto carrera) carrera)



--EJEMPLOS
ejMario :: Auto
ejMario = UnAuto "Rojo" 100 300
ejPeach :: Auto
ejPeach = UnAuto "Rosa" 50 150
ejDonko :: Auto
ejDonko = UnAuto "Marron" 70 200
carrera1 :: [Auto]
carrera1 = [ejMario, ejPeach, ejDonko]




