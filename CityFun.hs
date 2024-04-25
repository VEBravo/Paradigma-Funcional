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
esCiudadSobria ciudad num     | length (atracciones ciudad) == 0 = False
                              | otherwise = all (masLargoQueNum num) (atracciones ciudad)

masLargoQueNum:: Int -> String -> Bool
masLargoQueNum num palabra = length palabra > num

-- Ciudad con nombre raro
-- Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.

ciudadNombreRaro :: Ciudad -> Bool
ciudadNombreRaro ciudad = length (nombre ciudad) < 5 

-- Queremos poder registrar eventos que ocurren sobre una ciudad y que la afectan en mayor o menor medida. Dichos eventos son:

-- Sumar una nueva atracción
-- Queremos poder agregar una nueva atracción a la ciudad. Esto implica un esfuerzo de toda la comunidad en tiempo y dinero, lo que se traduce en un incremento del costo de vida de un 20%.

nuevaAtraccion :: Ciudad -> String -> Ciudad
nuevaAtraccion ciudad nuevaAtraccion = ciudad {atracciones = (atracciones ciudad) ++ [nuevaAtraccion], costoDeVida = costoDeVida ciudad * 1.2}

-- Crisis 
-- Al atravesar una crisis, la ciudad baja un 10% su costo de vida y se debe cerrar la última atracción de la lista.

atraviesaCrisis :: Ciudad -> Ciudad
atraviesaCrisis ciudad = ciudad {atracciones = init (atracciones ciudad), costoDeVida = costoDeVida ciudad * 0.9}

-- Remodelación 
-- Al remodelar una ciudad, incrementa su costo de vida un porcentaje que se indica al hacer la remodelación y le agrega el prefijo "New " al nombre.

remodelacionCiudad:: Ciudad -> Float -> Ciudad
remodelacionCiudad ciudad porcentaje = ciudad {nombre = "New " ++ nombre ciudad, costoDeVida = costoDeVida ciudad * (porcentaje/100 + 1) }

-- Se considera que el dato llega sin % desde el modulo que se encarga de la entrada salida

-- Reevaluación 
-- Si la ciudad es sobria con atracciones de más de n letras (valor que se quiere configurar), aumenta el costo de vida un 10%, si no baja 3 puntos.


reevaluacionCiudad:: Ciudad -> Int -> Ciudad
reevaluacionCiudad ciudad letras   |esCiudadSobria ciudad letras == True = ciudad {costoDeVida = costoDeVida ciudad * 1.10}
                                   |esCiudadSobria ciudad letras == False = ciudad {costoDeVida = costoDeVida ciudad - 3}

laTransformacionNoPara :: Ciudad -> Int -> Float -> Ciudad
laTransformacionNoPara ciudad letras porcentaje = (flip reevaluacionCiudad letras) (atraviesaCrisis (flip remodelacionCiudad porcentaje ciudad))

-- Ejemplos para testear

ejBaradero = UnaCiudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150
ejNullish = UnaCiudad "Nullish" 1800 [] 140
ejCaleta = UnaCiudad "Caleta Olivia" 1901 ["El Gorosito","Faro Costanera"] 120
ejMaipu = UnaCiudad "Maipú" 1878 ["Fortín Kakel"] 115
ejAzul = UnaCiudad "Azul" 1832 ["Teatro Español","Parque Municipal Sarmiento","Costanera Cacique Catriel"] 190
