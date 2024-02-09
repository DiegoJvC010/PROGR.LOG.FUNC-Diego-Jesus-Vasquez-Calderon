-- Ejercicio 1
descuento :: Double -> Double -> Double
descuento p d = p- (p * (d/100)) 

iva :: Double -> Double -> Double
iva p i= p * (1 + i/100) 

precioCesta :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
precioCesta cesta funcion = sum[funcion precio porcentaje | (precio, porcentaje) <- cesta]

-- Ejercicio 2
funcionAplicadaALista :: (a -> b) -> [a] -> [b]
funcionAplicadaALista _ [] = []
funcionAplicadaALista funcion (x:xs) = funcion x : funcionAplicadaALista funcion xs

raizCuadrada :: Floating a => a -> a
raizCuadrada n = sqrt n

-- Ejercicio 3
longitudFrase :: String -> [(String, Int)]
longitudFrase frase = [(palabra, length palabra)| palabra <- palabras]
    where palabras = words frase

-- Ejercicio 4
calificarAsignaturas :: [(String, Int)] -> [(String, String)]
calificarAsignaturas [] = []
calificarAsignaturas ((asignatura, nota):resto) =
    (toUpper asignatura, calificar nota) : calificarAsignaturas resto
    where
        toUpper = map toUpperChar
        toUpperChar c
            | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
            | otherwise = c

calificar :: Int -> String
calificar nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota <= 94  = "Notable"
    | nota >= 75 && nota <= 84  = "Bueno"
    | nota >= 70 && nota <= 74  = "Suficiente"
    | otherwise = "Desempeño insuficiente"

-- Ejercicio 5
moduloVector :: Floating a => [a] -> a
moduloVector v = sqrt (sum [x ** 2 | x <- v])

-- Ejercicio 6
media :: Fractional a => [a] -> a
media xs = sum xs / fromIntegral (length xs)

desviacionEstandar :: Floating a => [a] -> a
desviacionEstandar xs =
    let m = media xs
        diffs = map (\x -> (x - m) ^ 2) xs
    in sqrt (sum diffs / fromIntegral (length xs))

atipico :: (Ord a, Floating a) => [a] -> a -> Bool
atipico muestra n =
    let m = media muestra
        d = desviacionEstandar muestra
        puntuacion = (n - m) / d
    in puntuacion < -3 || puntuacion > 3

datosAtipicos :: (Ord a, Floating a) => [a] -> [a]
datosAtipicos muestra = filter (atipico muestra) muestra


