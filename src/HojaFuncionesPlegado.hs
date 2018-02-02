
module HojaFuncionesPlegado where

{-a)
-}
invertirLista :: [Int] -> [Int]
invertirLista = foldr (\x lista -> lista++[x]) []

--invetirListaOther :: [Int] -> [Int]
--invetirListaOther = foldr (++) [] -- falla
{- b)
-}

{- c)
-}


{- d)
-}
--Recibe una lista de enteros y devuelve la suma de sus dobles
sumaDobles :: [Int]->[Int]
sumaDobles l = foldr (\x lista-> [x+x]++lista ) [] l 
--Recibe una lista de enteros y devuelve la suma de sus cuadrados
sumaCuadrados :: [Int] -> [Int]
sumaCuadrados l = foldr(\x lista -> [x^2]++lista) [] l
--Recibe una lista de enteros y un entero y lo inserta al final de la lista
insertarAlFinal :: [Int] -> Int ->[Int]
insertarAlFinal l n = foldr(\x lista -> if lista == [] then lista else [x]++lista) [n] l
--Recibe una lista y un numero entero y devuelve la lista eliminando las aparciones
eliminarApariciones :: [Int] -> Int -> [Int]
eliminarApariciones l n = foldr(\x lista -> if x == n then lista else [x]++lista) [] l

eliminarAparicionesL :: [Int] -> Int -> [Int]
eliminarAparicionesL l n = foldl(\acc x -> if x == n then acc else x:acc) [] l

verdad :: [Bool] -> Bool
verdad = foldr (&&) True

