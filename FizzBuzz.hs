lessThan20 :: Int -> String
lessThan20 n |
    n > 0 && n < 20 =
    let answers = words ("One Two Three Four Five Six Seven Eight nine Ten Eleven Twelve Thirteen Fourteen Fifteen Sixteen Seventeen Eighteen Nineteen")
    in answers !! (n-1)

tens :: Int -> String
tens n | n >= 2 && n <= 9 = answers !! (n-2)
    where
        answers = words("Twenty Thirty Fourty Fifty Sixty Seventy Eighty Ninety")

number :: Int -> String
number n 
    | n  `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz!"
    | n  `mod` 5 == 0 = "Fizz!"
    | n  `mod` 3 == 0 = "Buzz!"
    | 1 <= n && n<20 = lessThan20(n) 
    | n `mod` 10 == 0 && n<100 = tens(n `div` 10)
    | n < 100 = tens(n `div` 10) ++ " " ++ lessThan20(n `mod` 10)

