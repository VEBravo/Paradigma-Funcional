type Actor = String
type Premio = Pelicula -> Bool
type Festival = [Premio]

-- MODELADO
data Pelicula = UnaPelicula {
    nombre :: String,
    reparto :: [Actor],
    duracion :: Int,
    anioEstreno :: Int
}

data Genero = UnGenero {
    genero :: String,
    actores :: [Actor]
}

-- FUNCIONES
esDeGenero :: Genero -> Pelicula -> Bool
esDeGenero genero pelicula = cuantosActuanEnGenero genero (reparto pelicula) > ((`div` 2).length) (reparto pelicula) 

esClasicoSetentista :: Premio
esClasicoSetentista pelicula = elem (anioEstreno pelicula) [1970..1979]

esDramaTenso :: Premio
esDramaTenso pelicula = esDeGenero drama pelicula && duracion pelicula > 180

esNsonMultitud :: Int -> Premio
esNsonMultitud n = (==n).length.reparto

esNsonEscazos :: Int -> Premio
esNsonEscazos n = not.esNsonMultitud n

esDeLaR :: Premio -- Si al menos 1 nombre de un actor empieza con la letra R
esDeLaR pelicula = any (\a -> (('R'==).head) a) (reparto pelicula)

cuantosPremiosRecibe :: Pelicula -> Festival -> Int
cuantosPremiosRecibe pelicula = length.filter (\premio -> premio pelicula)

-- AUXILIARES
cuantosActuanEnGenero :: Genero -> [Actor] -> Int
cuantosActuanEnGenero genero = length.filter (\a -> elem a (actores genero))

-- PELICULAS
taxiDriver = UnaPelicula "Taxi driver" ["De Niro","Foster"] 113 1976                -- 2 son multitud, clasicoSetentista
machete = UnaPelicula "Machete" ["De Niro","Rodriguez"] 105 2010                    -- 2 son multitud
harryPotter = UnaPelicula "Harry Potter 9" ["Watson","Radcliffe","Grint"] 1000 2022 -- 3 son multitud

-- GENEROS
comedia = UnGenero "Comedia" ["Carrey","Grint","Stiller"]
accion = UnGenero "Accion" ["Stallone","Willis","Schwarzenegger","De Niro"]
drama = UnGenero "Drama" ["De Niro","Foster"]

-- FESTIVALES
cannes :: Festival
cannes = [esNsonMultitud 3, esClasicoSetentista]
berlin :: Festival
berlin = [esNsonMultitud 4, esDramaTenso, esClasicoSetentista]
ennio :: Festival
ennio = [esClasicoSetentista, esNsonEscazos 3, esDeLaR]
