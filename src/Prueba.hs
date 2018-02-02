module Prueba where

{-
data Arbol a = AV |Nodo a (Arbol a) (Arbol a)

nivel::(Eq a) => Arbol a -> a-> Int
nivel AV n = 0
nivel (Nodo r hi hd) n = if r==n then 1
                         else aux + if aux==0 then 0 else 1 
                         where aux = max(nivel hi n, nivel hd n)
 
-} 
 
 
 
{-En un restaurante.... -}

data Ocupacion =   deriving show



insertarMesa::


ocuparMesa::