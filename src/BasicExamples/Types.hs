module BasicExamples.Types where

data Racional = R Int Int deriving Show

esEquivalente:: Racional->[Racional]->[Racional]
esEquivalente r [] = []
esEquivalente r (x:xs) = if eq r x then x:(esEquivalente r xs) else
 (esEquivalente r xs) 
 
-- r l = [x|x<-l, eq r x]
-- r l = foldl(\ acum x -> if eq r x then acum++[x] else acum) [] l


eq::Racional->Racional->Bool
eq (R n1 d1) (R n2 d2) = n1 * d2 == n2 * d1


--Dado un tipo de arbol, devuelva el número de nodos que tiene un árbol

data Arbol a = AV | Nodo a (Arbol a) (Arbol a) deriving Show

numNodos::Arbol a->Int
numNodos (AV) = 0
numNodos (Nodo r hi hd) = 1 + numNodos hi + numNodos hd
-- para probar (Nodo 7 (Nodo 8 AV (Nodo 12 AV AV))) (Nodo 9 AV AV)




