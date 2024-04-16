basicoPorCargo :: String -> Float
basicoPorCargo cargo
                 | cargo=="titular" = 149000
                 | cargo=="adjunto" = 116000
                 | cargo=="ayudante"= 66000

porcentajeIncremento :: Float -> Float
porcentajeIncremento anios 
    | anios>=24 = 120
    | anios>=10 = 50
    | anios>=5 = 30
    | anios>=3 = 20
    | otherwise = 0

porcentaje anios = 1 + (porcentajeIncremento anios)/100

bonificacionHoras :: Float -> Int
bonificacionHoras x | x<=50 && x>=5 = round (x/10)

sueldo :: String -> Float -> Float -> Float
sueldo cargo anios horas = basicoPorCargo cargo * porcentaje (anios) * fromIntegral (bonificacionHoras horas)

--TP PT 2
diferenciaFamiliar sueldo canasta paritaria inflacion = 
    (porcentaje paritaria) sueldo - porcentaje inflacion canasta

paraLlegarAFinDeMes cargo anios horas integrantes paritaria inflacion= 
    diferenciaFamiliar (sueldoDocente cargo anios horas) (canastaFamiliar integrantes) paritaria inflacion

canastaFamiliar 1 = 126000
canastaFamiliar 3 = 310000
canastaFamiliar 4 = 390000
canastaFamiliar 5 = 410000 
