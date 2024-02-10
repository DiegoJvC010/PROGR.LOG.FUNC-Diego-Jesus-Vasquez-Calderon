import Text.Read (Lexeme(String))
import Data.Char (toUpper)
-- Ejercicio 1
calculadora :: Double -> String -> [(Int, Double)]
calculadora valor funcion =
    case funcion of
        "seno" -> [(x, sin(fromIntegral x)) | x <- [1..truncate valor]]
        "coseno" -> [(x, cos(fromIntegral x)) | x <- [1..truncate valor]]
        "tangente" -> [(x, tan(fromIntegral x)) | x <- [1..truncate valor]]
        "exxponencial" -> [(x, exp(fromIntegral x)) | x <- [1..truncate valor]]
        "logaritmo neperiano" -> [(x, log(fromIntegral x)) | x <- [1..truncate valor]]
        _ -> error "Funcion no soportada"

imprimirValores :: [(Int,Double)] -> IO()
imprimirValores valores = do
    mapM_ (\(x,y) -> putStrLn (show x ++ "\t" ++ show y)) valores

{-main :: IO()
main = do
    putStrLn "Ingrese un entero positivo:"
    valorStr <- getLine
    let valor = read valorStr :: Double
    putStrLn "Ingrese la función (seno, coseno, tangente, exponencial, logaritmo neperiano):"
    funcion <- getLine
    let resultados = calculadora valor funcion
    imprimirValores resultados
-}
-- Ejercicio 2
esImpar :: Int -> Bool
esImpar valor = valor `mod` 2 /= 0

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar funcion lista = filter funcion lista

-- Ejercicio 3 
calificaciones :: [Int] -> [String]
calificaciones notas = map calificar notas
    where 
        calificar :: Int -> String
        calificar nota
            | nota>=95 && nota<=100 = "Excelente"
            | nota>=85 && nota<=94 = "Notable"
            | nota>=75 && nota<=84 = "Bueno"
            | nota>=70 && nota<=74 = "Suficiente"
            | otherwise = "Desemepeño insuficiente"

-- Ejercicio 4
calificarAlumno :: [(String, Int)] -> [(String, String)]
calificarAlumno asignaturasYNotas =
    let notasAprobadas = filter (\(_, nota) -> nota >=70) asignaturasYNotas
    in map (\(asignatura, nota) -> (map toUpper asignatura, calificaciones [nota] !! 0)) notasAprobadas

-- Ejercicio 5
data Inmueble = Inmueble { anio :: Int
                         , metros :: Int
                         , habitaciones :: Int
                         , garaje :: Bool
                         , zona :: Char
                        }

calcularPrecio :: Inmueble -> Float
calcularPrecio inmueble =
    let factorAntiguedad = 1 - fromIntegral(2024 - anio inmueble)/100
        precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
        precio = case zona inmueble of
            'A' -> precioBase * factorAntiguedad
            'B' -> precioBase * factorAntiguedad  * 1.5
            _ -> error "Zona no valida"
        in precio

buscarInmuebles :: [Inmueble] -> Float -> [Inmueble]
buscarInmuebles inmuebles presupuesto = filter (\inmueble -> calcularPrecio inmueble <= presupuesto) inmuebles

mostrarInmueble :: Inmueble -> String
mostrarInmueble (Inmueble anio metros habitaciones garaje zona) =
    "Inmueble { año = " ++ show anio ++ ", metros = " ++ show metros ++ ", habitaciones = " ++ show habitaciones ++ ", garaje = " ++ show garaje ++ ", zona = " ++ show zona ++ " }"

{-main :: IO()
main = do
    let pisos = [ Inmueble 2000 100 3 True 'A'
                , Inmueble 2012 60 2 True 'B'
                , Inmueble 1980 120 4 False 'A'
                , Inmueble 2005 75 3 True 'B'
                , Inmueble 2015 90 2 False 'A'
                ]
        presupuesto = 100000
        inmueblesEnPresupuesto = buscarInmuebles pisos (fromIntegral presupuesto)
    putStrLn $ "Inmuebles en presupuesto de $" ++ show presupuesto ++ ":"
    mapM_ (putStrLn . mostrarInmueble) inmueblesEnPresupuesto
-}
