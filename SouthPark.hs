data Personaje = UnPersonaje {
    nombre :: String,
    dinero :: Float,
    felicidad :: Float
}

ejButters :: Personaje
ejButters = UnPersonaje "Butters" 1000.50 10

irAEscuela :: Personaje -> Personaje
irAEscuela personaje | nombre personaje == "Butters" = personaje {felicidad = felicidad personaje + 20}
                     | otherwise = personaje {felicidad = felicidad personaje - 20}

comerCheesyPoof :: Personaje -> Personaje
comerCheesyPoof personaje = personaje {felicidad = felicidad personaje + 10, dinero = dinero personaje - 10}

irATrabajar :: Personaje -> String -> Personaje
irATrabajar personaje trabajo = personaje {dinero = dinero personaje + fromIntegral (length trabajo)}

hacerDobleTurno :: Personaje -> String -> Personaje
hacerDobleTurno personaje trabajo = personaje {dinero = 2*(dinero personaje + fromIntegral (length trabajo)), felicidad = felicidad personaje - fromIntegral(length trabajo)}

jugarWOW :: Personaje -> Int -> Float -> Personaje
jugarWOW personaje amigos horas | horas > 5 = personaje {felicidad = felicidad personaje + 50*fromIntegral amigos, dinero = dinero personaje - horas*10}
                                | otherwise = personaje {felicidad = felicidad personaje + 10*fromIntegral amigos*horas, dinero = dinero personaje - horas*10}

recibirseDeIngeniero :: Personaje -> Personaje
recibirseDeIngeniero personaje = personaje {nombre = "Ingeniero" ++ nombre personaje, felicidad = felicidad personaje + 1000, dinero = dinero personaje * 100}

