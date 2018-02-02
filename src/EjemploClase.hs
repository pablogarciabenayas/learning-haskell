module EjemploClase where

class Collection c where
 insertar:: c a -> a-> c a
 size:: c a -> Int
 eliminar:: c a -> c a
 primero::c a-> a
 esVacia::c a ->Bool
 
instance Collection Pila where
size (Pil l) = length l
insertar (Pil l) n = Pil (n:l)





instance Collection Cola where
insertar (Col l) n = Col l++[n]
eliminar (Col []) = Col[]


 
  