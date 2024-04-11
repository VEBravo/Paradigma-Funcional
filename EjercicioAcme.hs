--Misma cantidad de empleados en cada sucursal
cantEmpleados :: String -> Int
cantEmpleados x | x == "Acme" = 10
                | last x < head x = length x - 2
                | reverse x == x = (length x - 2) *2
                | mod (length x) 3 == 0 || mod (length x) 7 == 0 = 3
                | otherwise = 0
