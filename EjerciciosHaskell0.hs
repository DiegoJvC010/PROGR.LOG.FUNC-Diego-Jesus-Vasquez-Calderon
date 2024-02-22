import Data.Char (toLower)
-- Ejercicio 1 - Suma de elementos en una lista
sumarLista:: [Int] -> Int
sumarLista = sum

--Ejercicio 2 - Factorial
factorial :: Int -> Int
factorial n = product [1..n] 

-- Ejercicio 3 - Números pares
numerosPares :: Int -> [Int]
numerosPares n = takeWhile even [0, 2..n]

-- Ejercicio 4 - Longitud de una cadena
longitudCadena :: String -> Int
longitudCadena = length

-- Ejercicio 5 - Reverso de una lista
reversoLista :: [a] -> [a]
reversoLista = reverse

-- Ejercicio 6 - Duplicar elementos
duplicarElementos :: [Int] -> [Int]
duplicarElementos = concatMap (replicate 2)

-- Ejercicio 7 - Filtrar elementos pares
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

-- Ejercicio 8 - Fibonacci
fibonacci :: Int -> Int
fibonacci n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci(n-1) + fibonacci(n-2)

-- Ejercicio 9 - Divisores de un número
divisores :: Int -> [Int]
divisores n = [divisor | divisor <- [1..n], n `mod` divisor == 0] 

-- Ejercicio 10 - Palíndromo
esPalindromo :: String -> Bool
esPalindromo  cadena
    | map toLower cadena == reverse (map toLower cadena) = True
    | otherwise = False