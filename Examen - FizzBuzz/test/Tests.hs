module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FizzBuzz

fizzBuzzSuite :: TestTree
fizzBuzzSuite = testGroup "FizzBuzz tests"
                [ testGroup "Grupo 1" $
                    [ testCase "0 es cero" $ imprimirFizzBuzz 0 @?= "cero"
                    , testCase "1 es uno" $ imprimirFizzBuzz 1 @?= "uno"
                    , testCase "2 es FizzBuzz!" $ imprimirFizzBuzz 2 @?= "FizzBuzz!"
                    , testCase "3 es FizzBuzz!" $ imprimirFizzBuzz 3 @?= "FizzBuzz!"
                    , testCase "4 es cuatro" $ imprimirFizzBuzz 4 @?= "cuatro"
                    , testCase "5 es FizzBuzz!" $ imprimirFizzBuzz 5 @?= "FizzBuzz!"
                    , testCase "6 es seis" $ imprimirFizzBuzz 6 @?= "seis"
                    , testCase "7 es FizzBuzz!" $ imprimirFizzBuzz 7 @?= "FizzBuzz!"
                    , testCase "8 es ocho" $ imprimirFizzBuzz 8 @?= "ocho"
                    , testCase "9 es nueve" $ imprimirFizzBuzz 9 @?= "nueve"
                    , testCase "10 es diez" $ imprimirFizzBuzz 10 @?= "diez"
                    , testCase "11 es FizzBuzz!" $ imprimirFizzBuzz 11 @?= "FizzBuzz!"
                    , testCase "12 es doce" $ imprimirFizzBuzz 12 @?= "doce"
                    , testCase "13 es FizzBuzz!" $ imprimirFizzBuzz 13 @?= "FizzBuzz!"
                    , testCase "14 es catorce" $ imprimirFizzBuzz 14 @?= "catorce"
                    , testCase "15 es quince" $ imprimirFizzBuzz 15 @?= "quince"
                    , testCase "20 es veinte" $ imprimirFizzBuzz 20 @?= "veinte"
                    , testCase "23 es FizzBuzz!" $ imprimirFizzBuzz 23 @?= "FizzBuzz!"
                    , testCase "29 es FizzBuzz!" $ imprimirFizzBuzz 29 @?= "FizzBuzz!"
                    , testCase "30 es treinta" $ imprimirFizzBuzz 30 @?= "treinta"
                    ]
                    , testGroup "Grupo 2 (16-29)" $
                    [ testCase "16 es dieciseis" $ imprimirFizzBuzz 16 @?= "dieciseis"
                    , testCase "17 es FizzBuzz!" $ imprimirFizzBuzz 17 @?= "FizzBuzz!"
                    , testCase "18 es dieciocho" $ imprimirFizzBuzz 18 @?= "dieciocho"
                    , testCase "20 es veinte" $ imprimirFizzBuzz 20 @?= "veinte"
                    , testCase "21 es veintiuno" $ imprimirFizzBuzz 21 @?= "veintiuno"
                    , testCase "24 es veinticuatro" $ imprimirFizzBuzz 24 @?= "veinticuatro"
                    , testCase "26 es veintiseis" $ imprimirFizzBuzz 26 @?= "veintiseis"
                    , testCase "28 es veintiocho" $ imprimirFizzBuzz 28 @?= "veintiocho"
                    , testCase "29 es FizzBuzz!" $ imprimirFizzBuzz 29 @?= "FizzBuzz!"
                    ]
                    , testGroup "Grupo 3 (31-100)"
                    [ testCase "30 es treinta" $ imprimirFizzBuzz 30 @?= "treinta"
                    , testCase "31 es FizzBuzz!" $ imprimirFizzBuzz 31 @?= "FizzBuzz!"
                    , testCase "37 es FizzBuzz!" $ imprimirFizzBuzz 37 @?= "FizzBuzz!"
                    , testCase "40 es cuarenta" $ imprimirFizzBuzz 40 @?= "cuarenta"
                    , testCase "43 es FizzBuzz!" $ imprimirFizzBuzz 43 @?= "FizzBuzz!"
                    , testCase "50 es cincuenta" $ imprimirFizzBuzz 50 @?= "cincuenta"
                    , testCase "60 es sesenta" $ imprimirFizzBuzz 60 @?= "sesenta"
                    , testCase "61 es FizzBuzz!" $ imprimirFizzBuzz 61 @?= "FizzBuzz!"
                    , testCase "70 es setenta" $ imprimirFizzBuzz 70 @?= "setenta"
                    , testCase "80 es ochenta" $ imprimirFizzBuzz 80 @?= "ochenta"
                    , testCase "90 es noventa" $ imprimirFizzBuzz 90 @?= "noventa"
                    , testCase "100 es cien" $ imprimirFizzBuzz 100 @?= "cien"
                    ]
                    , testGroup "Grupo 4 (101-999)"
                    [ testCase "105 es ciento cinco" $ imprimirFizzBuzz 105 @?= "ciento cinco"
                    , testCase "106 es ciento seis" $ imprimirFizzBuzz 106 @?= "ciento seis"
                    , testCase "107 es FizzBuzz!" $ imprimirFizzBuzz 107 @?= "FizzBuzz!"
                    , testCase "108 es ciento ocho" $ imprimirFizzBuzz 108 @?= "ciento ocho"
                    , testCase "157 es FizzBuzz!" $ imprimirFizzBuzz 157 @?= "FizzBuzz!"
                    , testCase "158 es ciento cincuenta y ocho" $ imprimirFizzBuzz 158 @?= "ciento cincuenta y ocho"
                    , testCase "159 es ciento cincuenta y nueve" $ imprimirFizzBuzz 159 @?= "ciento cincuenta y nueve"
                    , testCase "160 es ciento sesenta" $ imprimirFizzBuzz 160 @?= "ciento sesenta"
                    , testCase "200 es doscientos" $ imprimirFizzBuzz 200 @?= "doscientos"
                    , testCase "109 es FizzBuzz!" $ imprimirFizzBuzz 109 @?= "FizzBuzz!"
                    , testCase "287 es doscientos ochenta y siete" $ imprimirFizzBuzz 287 @?= "doscientos ochenta y siete"
                    , testCase "288 es doscientos ochenta y ocho" $ imprimirFizzBuzz 288 @?= "doscientos ochenta y ocho"
                    , testCase "289 es doscientos ochenta y nueve" $ imprimirFizzBuzz 289 @?= "doscientos ochenta y nueve"
                    , testCase "290 es doscientos noventa" $ imprimirFizzBuzz 290 @?= "doscientos noventa"
                    , testCase "293 es FizzBuzz!" $ imprimirFizzBuzz 293 @?= "FizzBuzz!"
                    , testCase "300 es trescientos" $ imprimirFizzBuzz 300 @?= "trescientos"
                    , testCase "400 es cuatrocientos" $ imprimirFizzBuzz 400 @?= "cuatrocientos"
                    , testCase "500 es quinientos" $ imprimirFizzBuzz 500 @?= "quinientos"
                    , testCase "600 es seiscientos" $ imprimirFizzBuzz 600 @?= "seiscientos"
                    , testCase "700 es setecientos" $ imprimirFizzBuzz 700 @?= "setecientos"
                    , testCase "800 es ochocientos" $ imprimirFizzBuzz 800 @?= "ochocientos"
                    , testCase "999 es novecientos noventa y nueve" $ imprimirFizzBuzz 999 @?= "novecientos noventa y nueve"
                    ]
                    , testGroup "Grupo 5 (1000-999999)"
                    [ testCase "10870 es diez mil ochocientos setenta" $ imprimirFizzBuzz 10870 @?= "diez mil ochocientos setenta"
                    , testCase "104729 es FizzBuzz!" $ imprimirFizzBuzz 104729 @?= "FizzBuzz!"
                    , testCase "203521 es doscientos tres mil quinientos veintiuno" $ imprimirFizzBuzz 203521 @?= "doscientos tres mil quinientos veintiuno"
                    , testCase "503621 es FizzBuzz!" $ imprimirFizzBuzz 503621 @?= "FizzBuzz!"
                    , testCase "709289 es setecientos nueve mil doscientos ochenta y nueve" $ imprimirFizzBuzz 709289 @?= "setecientos nueve mil doscientos ochenta y nueve"
                    , testCase "912839 es FizzBuzz!" $ imprimirFizzBuzz 912839 @?= "FizzBuzz!"
                    , testCase "221000 es doscientos veintiun mil" $ imprimirFizzBuzz 221000 @?= "doscientos veintiun mil"
                    , testCase "101890 es ciento un mil ochocientos noventa" $ imprimirFizzBuzz 101890 @?= "ciento un milochocientos noventa"
                    , testCase "999999 es novecientos noventa y nueve mil novecientos noventa y nueve" $ imprimirFizzBuzz 999999 @?= "novecientos noventa y nueve mil novecientos noventa y nueve"
                    ]
                    , testGroup "Grupo 6"
                    [ testCase "1000000 es un millon" $ imprimirFizzBuzz 1000000 @?= "un millon"
                    ]
                ]

main = defaultMain fizzBuzzSuite