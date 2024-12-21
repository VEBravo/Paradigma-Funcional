type Excursion = Turista -> Turista
type Indice = Turista -> Float
type Tour = [Excursion]

solo :: Bool
solo = True
acompaniado :: Bool
acompaniado = False

data Turista = UnTurista{
    cansancio :: Float,
    stress :: Float,
    comoViaja :: Bool,
    idiomas :: [String]
} deriving (Show,Eq)

ana = UnTurista 0 21 acompaniado ["espanol"]
beto = UnTurista 15 15 solo ["aleman"]
cathi = UnTurista 15 15 solo ["aleman","catalan"]


irALaPlaya :: Excursion
irALaPlaya turista | comoViaja turista = turista {cansancio = cansancio turista - 5}
                   | otherwise = turista {stress = stress turista - 1}

apreciarElemento :: String -> Excursion
apreciarElemento nombre turista = turista{stress = stress turista - fromIntegral(length nombre)}

hablarIdioma :: String -> Excursion
hablarIdioma idioma turista = turista {comoViaja = acompaniado, idiomas = idiomas turista ++ [idioma]}

caminar :: Float -> Excursion
caminar minutos turista = turista{stress = stress turista - intensidad, cansancio = cansancio turista + intensidad}
                          where intensidad = minutos / 4

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista | marea == "fuerte" = turista {stress = stress turista + 6, cansancio = cansancio turista + 10}
                           | marea == "tranquila" = ((hablarIdioma "aleman").(apreciarElemento "mar").(caminar 10)) turista
                           | otherwise = turista

modificarStress :: Float -> Turista -> Turista
modificarStress porcentaje turista = turista {stress = stress turista*porcentaje}

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = ((modificarStress 0.9).excursion)

deltaSegun :: (a -> Float) -> a -> a -> Float
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Float
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (fromIntegral.length.idiomas) turista excursion /= 0

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante excursion turista = (deltaExcursionSegun (stress) turista excursion) <= (-3)

completo :: Tour
completo = [caminar 20, apreciarElemento "cascada", caminar 40, irALaPlaya, hablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB eleccion = [paseoEnBarco "tranquila", eleccion, caminar 120]

islaVecina :: String -> Tour
islaVecina marea = [paseoEnBarco marea, excursion, paseoEnBarco marea]
                 where excursion | marea == "fuerte" = apreciarElemento "lago"
                                 | otherwise = irALaPlaya

hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (\turista excursion -> hacerExcursion excursion turista) turista {stress = stress turista + (fromIntegral.length) tour} tour

-- esConvincente :: Turista -> Tour -> Bool
-- esConvincente turista tour = any (\excursion -> esDesestresante excursion turista && ((==acompaniado).comoViaja.(hacerExcursion excursion)) turista) tour

-- algunoEsConvincente :: Turista -> [Tour] -> Bool
-- algunoEsConvincente turista = any esConvincente

-- espiritualidad :: Tour -> Turista -> Float
-- espiritualidad tour turista = (sum.(map (\excursion -> deltaExcursionSegun stress turista excursion + deltaExcursionSegun cansancio turista excursion))) tour

-- efectividad :: Tour -> [Turista] -> Float
-- efectividad tour turistas = (sum.(map (\turista -> espiritualidad tour turista)).(filter (\turista -> esConvincente turista tour)))

tourInfinito :: Tour
tourInfinito = repeat irALaPlaya