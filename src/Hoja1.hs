module Hoja1 where

import Data.Char

{- a) Implementar una función en haskell que dados tres números enteros
		determine si están ordenados de menor a mayor.
-}

estanOrdenados :: Int -> Int ->Int -> Bool
estanOrdenados x y z = (x < y && y < z)

{- b) Implementar una función en haskell que dados tres números enteros
		los devuelva ordenados de menor a mayor
-}
{--
mayor :: Int->Int->Int
mayor x y = if x>y then x else y

ordenarTres :: Int -> Int -> Int -> (Int,Int,Int)
ordenarTres x y z =
	|
	|
-}

{- c) Implementar una función que reciba un número real y devuelva
		una tupla con su parte entera y sus dos primeros decimales
		(como número entero)
-} 

parteEnteraDecimal :: Double -> (Integer,Integer)
parteEnteraDecimal x = (truncate x, truncate ((x - fromInteger(truncate x))*100))


{- d) 
-}
longitudArea :: Double -> (Double,Double)
longitudArea x= (2*p*x,p*x*x)
 where
  p = 3.1415
 
longitudAreaLet :: Double -> (Double,Double)
longitudAreaLet x =
 let p = 3.1415 
  in (2*p*x,p*x*x)
 
{- e)
-}
concatenar :: [Integer]->[Integer]->[Integer]
concatenar [] [] = []
concatenar l1 l2 = [j| i <-l1, j<-l2]

{- f)
-}
factores :: Int -> [Int]
factores n = [e | e <- [1..n], e`mod`n == 0 ]

{- g)
-}
esPrimo :: Int -> Bool
esPrimo n = length (factores n) == 2

{- h)
-} 
cuantasMayusculas :: [Char] -> Int
cuantasMayusculas s = length([c | c <-s, isUpper c])

{- i)
-}


{- j)
-}
sumaCuatroPrimeros :: [Int] -> Bool
sumaCuatroPrimeros [] = True
sumaCuatroPrimeros (w:x:y:z:ns) = (w+x+y+z) < 10
sumaCuatroPrimeros (x:y:z:ns) = (x+y+z) < 10
sumaCuatroPrimeros (y:z:ns) = (y+z) < 10
sumaCuatroPrimeros (z:ns) = z < 10

{- k)
-}
puntoCardinal :: Char ->String
puntoCardinal 'N' = "Norte"
puntoCardinal 'S' = "Sur"
puntoCardinal 'E' = "Este"
puntoCardinal 'O' = "Oeste"

{- l)
-}
primeraYultimaLetra :: String -> String
primeraYultimaLetra l = "La primera letra de la frase " ++ l ++
 " es "++ (take 1 l) ++" y la ultima letra es " ++ (drop (length(l)-1) l)


{- m)
-}
clasificarValorEntrada :: Integer ->String
clasificarValorEntrada n
 |n < 10 = menor_10
 |n >= 10 && n <= 20 = entre10_y20
 | n > 20 = mayor_20
  where 
   menor_10 = "El valor de entrada es menor que 10"
   entre10_y20 = "El valor de entrada es mayor o igual a 10 y menor o igual a 20"
   mayor_20 = "El valor de entrada es mayor que 20"

{-n)
-}
existeChar :: String -> Char -> Int
existeChar [] _ = 0
existeChar str c = length([x | x <- str, x==c])


