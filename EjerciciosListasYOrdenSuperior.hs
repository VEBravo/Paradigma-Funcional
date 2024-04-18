sumaLista :: [Int] -> Int
sumaLista = sum

--2)
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
promedioFrecuenciaCardiaca :: [Int]-> Float
promedioFrecuenciaCardiaca lista =  fromIntegral (sum lista) / fromIntegral (length lista)

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto x = frecuenciaCardiaca !! (div x 10)

frecuenciaHastaMomento :: Int -> [Int]
frecuenciaHastaMomento momento = take (div momento 10 + 1) frecuenciaCardiaca

--3)
esCapicua :: [String] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

--4)
duracionLlamadas :: ((String,[Int]), (String,[Int]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]), ("horarioNormal",[10,5,8,2,9,10]))
cuandoHabloMasMinutos :: ((String,[Int]), (String,[Int])) -> String
cuandoHabloMasMinutos ((reduc,listaReduc), (normal,listaNorm)) | sum listaReduc > sum listaNorm = reduc
                                                               | sum listaReduc < sum listaNorm = normal

cuandoHizoMasLlamadas :: ((String,[Int]), (String,[Int])) -> String
cuandoHizoMasLlamadas ((reduc,listaReduc), (normal,listaNorm)) | length listaReduc > length listaNorm = reduc
                                                               | length listaReduc < length listaNorm = normal