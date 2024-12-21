factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

g :: Float -> Float -> Float
g 0 x = 0
g y 0 = y
g y x = x + y

cuantoTeQuiero :: String -> Float
cuantoTeQuiero "Luchin" = 5/0



