import Data.List (sort)

-- Lo sacamos de la guia ↑↑ para poder usar sort

-- Punto 1: Valor de una ciudad
-- Valor de una ciudad 
-- Definir el valor de una ciudad, un número que se obtiene de la siguiente manera:
-- si fue fundada antes de 1800, su valor es 5 veces la diferencia entre 1800 y el año de fundación
-- si no tiene atracciones, su valor es el doble del costo de vida
-- de lo contrario, será 3 veces el costo de vida de la ciudad
data Ciudad = UnaCiudad {
    nombre :: String,
    anioFundacion :: Int,
    atracciones :: [String],
    costoDeVida :: Float
} deriving (Show)

valorCiudad :: Ciudad -> Float
valorCiudad (UnaCiudad _ anioFundacion atracciones costoDeVida) 
    | anioFundacion < 1800 = 5*(1800 - fromIntegral anioFundacion)
    | length atracciones == 0 = 2 * costoDeVida
    | otherwise = 3 * costoDeVida

-- Características de las ciudades
-- Alguna atracción copada
-- Queremos saber si una ciudad tiene alguna atracción copada, esto es que la atracción comience con una vocal. 
-- Por ejemplo: "Acrópolis" es una atracción copada y "Golden Gate" no es copada.

atraccionCopada:: Ciudad -> Bool
atraccionCopada ciudad = any inicialVocal (atracciones ciudad)

inicialVocal:: String -> Bool
inicialVocal palabra = head palabra `elem` "aeiouAEIOUáéíóúÁÉÍÓÚ"

-- Ciudad sobria
-- Queremos saber si una ciudad es sobria, esto se da si todas las atracciones tienen más de x letras.
-- El valor x tiene que poder configurarse

esCiudadSobria:: Ciudad -> Int -> Bool
esCiudadSobria ciudad num     | length (atracciones ciudad) == 0 = False -- Se repite la logica -> condicion && condicion && evitar guardas
                              | otherwise = all (masLargoQueNum num) (atracciones ciudad) -- 2 f una tiene atracciones y la otra tiene mas de null

masLargoQueNum:: Int -> String -> Bool
masLargoQueNum num palabra = length palabra > num

-- Ciudad con nombre raro
-- Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.

ciudadNombreRaro :: Ciudad -> Bool
ciudadNombreRaro ciudad = length (nombre ciudad) < 5 

-- Queremos poder registrar eventos que ocurren sobre una ciudad y que la afectan en mayor o menor medida. Dichos eventos son:

-- Sumar una nueva atracción
-- Queremos poder agregar una nueva atracción a la ciudad. Esto implica un esfuerzo de toda la comunidad en tiempo y dinero, lo que se traduce en un incremento del costo de vida de un 20%.

nuevaAtraccion :: String -> Ciudad -> Ciudad
nuevaAtraccion atraccionNueva ciudad = ciudad {atracciones = (atracciones ciudad) ++ [atraccionNueva], costoDeVida = costoDeVida ciudad * 1.2}

-- Crisis 
-- Al atravesar una crisis, la ciudad baja un 10% su costo de vida y se debe cerrar la última atracción de la lista.

atraviesaCrisis :: Ciudad -> Ciudad
atraviesaCrisis ciudad | length (atracciones ciudad) == 0 = ciudad {costoDeVida = costoDeVida ciudad * 0.9}
                       | otherwise = ciudad {atracciones = init (atracciones ciudad), costoDeVida = costoDeVida ciudad * 0.9}
-- Remodelación 
-- Al remodelar una ciudad, incrementa su costo de vida un porcentaje que se indica al hacer la remodelación y le agrega el prefijo "New " al nombre.

remodelacionCiudad:: Float -> Ciudad -> Ciudad
remodelacionCiudad porcentaje ciudad = ciudad {nombre = "New " ++ nombre ciudad, costoDeVida = costoDeVida ciudad * (porcentaje/100 + 1) }

-- Se considera que el dato llega sin % desde el modulo que se encarga de la entrada salida

-- Reevaluación 
-- Si la ciudad es sobria con atracciones de más de n letras (valor que se quiere configurar), aumenta el costo de vida un 10%, si no baja 3 puntos.


reevaluacionCiudad:: Int -> Ciudad -> Ciudad
reevaluacionCiudad letras ciudad   |esCiudadSobria ciudad letras = ciudad {costoDeVida = costoDeVida ciudad * 1.10}
                                   |otherwise = ciudad {costoDeVida = costoDeVida ciudad - 3}
--Con la misma condicion es preferible poner un otherwise
laTransformacionNoPara :: Ciudad -> Int -> Float -> Ciudad
laTransformacionNoPara ciudad letras porcentaje = (reevaluacionCiudad letras . atraviesaCrisis . remodelacionCiudad porcentaje) ciudad
-- Cambiar orden parametros
-- Punt 4
-- 4.1 Los años pasan... 
-- Queremos modelar un año, donde definamos
-- el número que le corresponde
-- una serie de eventos que se produjeron
-- También queremos reflejar el paso de un año para una ciudad, es decir, que los eventos afecten el estado final en el que queda una ciudad.

data Anio = UnAnio {
    numero :: Int,
    eventos :: [(Ciudad->Ciudad)]
}

reflejarAnio :: Ciudad -> Anio -> Ciudad
reflejarAnio ciudad anio = foldl (\ciudad funcion -> funcion ciudad) ciudad (eventos anio)

-- 4.2  Algo mejor 
-- Implementar una función que reciba una ciudad, un criterio de comparación y un evento, 
-- de manera que nos diga si la ciudad tras el evento subió respecto a ese criterio. 


-- Ord es un tipo de dato ordenable que permite la comparación. Por ejemplo Int, String, Float, Char son todos tipos de datos comparables.
-- El debugger nos recomendó agregar Ord a para poder hacer la comparación entre los criterios.
subioPostEvento :: Ord a => Ciudad -> (Ciudad -> a) -> (Ciudad -> Ciudad) -> Bool
subioPostEvento ciudad criterio evento = criterio (evento ciudad) > criterio ciudad

-- 4.3 Costo de vida que suba 
-- Para un año, queremos aplicar sobre una ciudad solo los eventos que hagan que el costo de vida suba. 
-- Debe quedar como resultado la ciudad afectada con dichos eventos.
costoDeVidaQueSuba :: Ciudad -> Anio -> Ciudad
costoDeVidaQueSuba ciudad anio = foldl (\ciudad funcion -> funcion ciudad) ciudad (filter (subioCostoVida ciudad) (eventos anio))

subioCostoVida :: Ciudad -> (Ciudad -> Ciudad) -> Bool
subioCostoVida ciudad evento = costoDeVida (evento ciudad) > costoDeVida ciudad

-- 4.4 Costo de vida que baje  
-- Para un año, queremos aplicar solo los eventos que hagan que el costo de vida baje. Debe quedar como resultado la ciudad afectada con dichos eventos.
costoDeVidaQueBaje :: Ciudad -> Anio -> Ciudad
costoDeVidaQueBaje ciudad anio = foldl (\ciudad funcion -> funcion ciudad) ciudad (filter (bajaCostoVida ciudad) (eventos anio))

bajaCostoVida :: Ciudad -> (Ciudad -> Ciudad) -> Bool
bajaCostoVida ciudad evento = costoDeVida (evento ciudad) < costoDeVida ciudad

-- 4.5 Valor que suba  
-- Para un año, queremos aplicar solo los eventos que hagan que el valor suba. Debe quedar como resultado la ciudad afectada con dichos eventos.
valorQueSuba :: Ciudad -> Anio -> Ciudad
valorQueSuba ciudad anio = foldl (\ciudad funcion -> funcion ciudad) ciudad (filter (subioValor ciudad) (eventos anio))

subioValor :: Ciudad -> (Ciudad -> Ciudad) -> Bool
subioValor ciudad evento = valorCiudad (evento ciudad) > valorCiudad ciudad
-- En esta función en vez de 161.7 nos dio 147 el Costo de vida de Nullish, no sabemos por qué

-- 5.1 Eventos ordenados 
-- Dado un año y una ciudad, queremos saber si los eventos están ordenados en forma correcta, 
-- esto implica que el costo de vida al aplicar cada evento se va incrementando respecto al anterior evento. Debe haber al menos un evento para dicho año.
eventosOrdenados :: Anio -> Ciudad -> Bool
eventosOrdenados anio ciudad = obtenerListaCostoDeVida anio ciudad == sort (obtenerListaCostoDeVida anio ciudad)

obtenerListaCostoDeVida :: Anio -> Ciudad -> [Float]
obtenerListaCostoDeVida anio ciudad = map (costoDeVida . (\funcion -> funcion ciudad)) (eventos anio)

-- 5.2 Dado un evento y una lista de ciudades, queremos saber si esa lista está ordenada. 
-- Esto implica que el costo de vida al aplicar el evento sobre cada una de las ciudades queda en orden creciente. 
-- Debe haber al menos una ciudad en la lista.
ciudadesOrdenadas :: (Ciudad -> Ciudad) -> [Ciudad] -> Bool
ciudadesOrdenadas evento listaCiudades = map (costoDeVida.evento) listaCiudades == sort (map (costoDeVida.evento) listaCiudades)

-- 5.3 Años ordenados
-- Dada una lista de años y una ciudad, queremos saber si el costo de vida al aplicar todos los eventos de cada año sobre esa ciudad termina generando una serie de costos de vida ascendente 
-- (de menor a mayor). Debe haber al menos un año en la lista.

aniosOrdenados :: [Anio] -> Ciudad -> Bool
aniosOrdenados anios ciudad | not (null anios) = map (costoDeVida . reflejarAnio ciudad) anios == sort (map (costoDeVida . reflejarAnio ciudad) anios)

--f = flip remodelacionCiudad 5 -- :o Esto es algo que recibe una ciudad y devuelve una ciudad
-- type evento 

-- Ejemplos para testear

ejBaradero :: Ciudad
ejBaradero = UnaCiudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150
ejNullish :: Ciudad
ejNullish = UnaCiudad "Nullish" 1800 [] 140
ejCaleta = UnaCiudad "Caleta Olivia" 1901 ["El Gorosito","Faro Costanera"] 120
ejMaipu :: Ciudad
ejMaipu = UnaCiudad "Maipú" 1878 ["Fortín Kakel"] 115
ejAzul = UnaCiudad "Azul" 1832 ["Teatro Español","Parque Municipal Sarmiento","Costanera Cacique Catriel"] 190

ej2022 :: Anio
ej2022 = UnAnio 2022 [atraviesaCrisis, reevaluacionCiudad 7]
ej2021 :: Anio
ej2021 = UnAnio 2021 [atraviesaCrisis, nuevaAtraccion "playa"]
ej2023 :: Anio
ej2023 = UnAnio 2022 [atraviesaCrisis, nuevaAtraccion "parque", remodelacionCiudad 10, remodelacionCiudad 20]
ej2024 :: Anio
ej2024 = UnAnio 2024 (eventosInfinitos atraviesaCrisis)

-- Una serie de eventos interminables 
-- Definir el año 2024 con una lista infinita de eventos.

eventosInfinitos :: (Ciudad -> Ciudad) -> [Ciudad->Ciudad]
eventosInfinitos evento = evento:eventosInfinitos evento

-- Puede haber un resultado posible para la función del punto 5.1 (eventos ordenados) para el año 2024? Justificarlo relacionándolo con conceptos vistos en la materia

-- En nuestro caso, al utilizar la función sort es imposible conseguir el resultado porque necesita recorrer toda la lista para saber si están ordenados.
-- Si hubiesemos utilizado otra función que utilice el valor actual y el próximo podríamos obtener que la lista no está ordenada si en algun momento encuentra
-- dos elementos que no cumplan con la condición de estar ordenados debido a la evaluación diferida.
-- Pero si la lista es infinita nunca podríamos decir que sí está ordenada ya que nunca terminaríamos de recorrerla.

-- Puede haber un resultado posible para la función del punto 5.2 (ciudades ordenadas) para el año 2024? Justificarlo relacionándolo con conceptos vistos en la materia?

-- En este caso, no se utiliza un año, por lo que al no encontrar una lista infinita no habrá problemas con el sort. 

-- Puede haber un resultado posible para la función del punto 5.3 (años ordenados) para el año 2024? Justificarlo relacionándolo con conceptos vistos en la materia?

-- Al utilizar sort, por lo explicado antes, en nuestro caso no es posible. A su vez, no sería posible definir el valor de la función ya que, al encontrar el año 2024 en la
-- lista de años, nunca terminaría el compilador de aplicarle todos los eventos, por lo que no sería posible luego evaluar la lista de costos de vida. 

-- CONCLUSIÓN: Solo es posible decir si una lsita está desordenada, siempre que se utilice una función recursiva que relacione el elemento actual con el siguiente, algo que con
-- la funcion sort no es posible. Luego nunca se podrá afirmar que una lista infinita está ordenada, ya que implicaría recorrerla por completo.
