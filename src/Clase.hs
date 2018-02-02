
module Clase where

class Coleccion c where
 insertar:: c a -> a -> c a
 size:: c a -> Int
 eliminar::c a-> c a
 primero::c a-> a
 esVacia::c a -> Bool

instance Coleccion Pila where
 size (Pil l) = length l
 insertar (Pil l) n = Pil (n:l)
 eliminar (Pil []) = Pil []
 eliminar (Pil x:xs) = Pil xs
 primero (Pil l) = head l
 esVacia (Pil []) = true
esVacia (Pil x:xs) = false

instance Coleccion Cola where
 insertar (Col l) n = Col l ++ n
 eliminar (Col []) = Col []
 eliminar (Col x:xs) = Col xs
 primero (Col l) = head l
 esVacia (Col []) = true
 esVacia (Col x:xs) = false