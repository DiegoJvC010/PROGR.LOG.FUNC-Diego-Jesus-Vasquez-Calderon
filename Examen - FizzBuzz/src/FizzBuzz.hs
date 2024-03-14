module FizzBuzz where

-- Función principal para imprimir FizzBuzz o el número en letras
imprimirFizzBuzz :: Int -> String
imprimirFizzBuzz n = if esPrimo n then "FizzBuzz!" else numeroEnLetras n

-- Función para convertir un número en letras
numeroEnLetras :: Int -> String
numeroEnLetras n
    -- Caso base: números del 0 al 19
    | n < 20 = digitos !! n
    -- Números del 20 al 99
    | n < 100 = let (decena, unidad) = n `divMod` 10
                in if unidad == 0 then decenas !! decena
                   else if decena == 2
                        then "veinti" ++ digitos !! unidad
                        else decenas !! decena ++ " y " ++ digitos !! unidad
    -- Números del 100 al 999
    | n < 1000 =
        let (centena, resto) = n `divMod` 100
            centenaStr = if centena == 1 && resto /= 0 then "ciento"
                         else centenas !! centena
        in if resto == 0 then centenaStr
           else centenaStr ++ " " ++ numeroEnLetras resto
    -- Números del 1000 al 1999
    | n == 1000 = "mil"
    | n < 2000 = "mil " ++ numeroEnLetras (n `mod` 1000)
    -- Números del 2000 al 999999
    | n < 100000 =
        let (miles, resto) = n `divMod` 1000
            milesStr
              | miles == 21 = "veintiun mil"
              | miles == 31 = "treinta y un mil"
              | miles == 41 = "cuarenta y un mil"
              | miles == 51 = "cincuenta y un mil"
              | miles == 61 = "sesenta y un mil"
              | miles == 71 = "setenta y un mil"
              | miles == 81 = "ochenta y un mil"
              | miles == 91 = "noventa y un mil"
              | miles >= 20 && miles `mod` 10 == 0 = decenas !! (miles `div` 10) ++ " milo"
              | miles >= 20 = if miles `div` 10 == 2 
                then decenas !! 1 ++ numeroEnLetras (miles `mod` 10) ++ " mil"
                else decenas !! (miles `div` 10) ++ " y " ++ numeroEnLetras (miles `mod` 10) ++ " mil"
              | otherwise = digitos !! miles ++ " mil"
        in if resto == 0 then milesStr
           else milesStr ++ " " ++ numeroEnLetras resto
    | n < 1000000 =
        let (miles, resto) = n `divMod` 1000
            milesStr
              | miles == 100 = "cien mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 101 = "ciento un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 201 = "doscientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 301 = "trescientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 401 = "cuatrocientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 501 = "quimientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 601 = "seiscientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 701 = "setecientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 801 = "ochocientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""
              | miles == 901 = "novecientos un mil" ++ if resto /= 0 then numeroEnLetras resto else ""

              | miles > 100 && miles < 200 = "ciento " ++ numeroEnLetras(n `mod` 100000)
              | miles >= 200 = centenas !! (n `div` 100000) ++" "++ numeroEnLetras(n `mod` 100000)
              | otherwise = digitos !! miles ++ ""
        in milesStr
    -- Un millon
    | n == 1000000 = "un millon"
    | otherwise = "Número fuera de rango"


-- Lista de los números en letras del 0 al 19
digitos :: [String]
digitos = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece", "catorce", "quince", "dieciseis", "diecisiete", "dieciocho", "diecinueve"]

-- Lista de las decenas en letras
decenas :: [String]
decenas = ["", "veinti", "veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

-- Lista de las centenas en letras
centenas :: [String]
centenas = ["", "cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]

-- Función para verificar si un número es primo
esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]
