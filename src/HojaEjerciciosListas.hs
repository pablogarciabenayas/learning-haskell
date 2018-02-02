
module HojaEjerciciosListas where

{- a)
-}
sacaEnteros :: [Int] -> [Int]
sacaEnteros [] = []
sacaEnteros [0] = []
sacaEnteros (x:xs) = [x]++ sacaEnteros xs 


{- b)
-}

{- c)
-}
listaSecuencias :: [Int] -> [[Int]]
listaSecuencias [] = []
listaSecuencias (x:xs) = listaSecuencias xs:x 
{- d)
-}

{- e)
-}




