
module Hoja2 where
import Data.Char

{- a)
-}
cribar1 :: [Int]->Int->[Int]
cribar1 [] _ = []
cribar1 (n:ns) x = if (n `mod` x) == 0 then cribar1 ns x else n:(cribar1 ns x)
 
 
 --Listas por comprension
cribar2 :: [Int]->Int->[Int]
cribar2 l x = [ n|n <- l, n`mod`x /= 0]
 
 -- Recursividad final
cribar3 :: [Int]->Int->[Int]
cribar3 l x = criba3Aux l x []
 
 --Función auxiliar para el caso de recursividad final
criba3Aux:: [Int] -> Int ->[Int]->[Int]
criba3Aux [] _ acum = acum
criba3Aux (n:ns) x acum = if n `mod` x==0 then criba3Aux ns x acum else
 criba3Aux ns x (acum++[n])
{- b)
-}
doble :: Int -> Int
doble x = x + x

-- función lambda \x-> x*2

{- c)
-}
sumaDobleRecursivoNoFinal :: [Int] -> Int
sumaDobleRecursivoNoFinal [] = 0
sumaDobleRecursivoNoFinal (n:ns) = 2 * n + sumaDobleRecursivoNoFinal ns

sumaDobleRecursivaFinal :: [Int] -> Int
sumaDobleRecursivaFinal (n:ns) = sumaDobleRecursivaAcum ns 0

sumaDobleRecursivaAcum :: [Int] -> Int-> Int
sumaDobleRecursivaAcum [] acum = acum
sumaDobleRecursivaAcum (x:xs) acum = sumaDobleRecursivaAcum xs acum+2*x 

sumaDobleLambda :: [Int] -> Int
sumaDobleLambda l = sum (map (*2)l)

{- d)
-}
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares l = sum(map(^2) (filter even l))

sumaCuadradosParesComprension :: [Int] -> Int
sumaCuadradosParesComprension l = sum([x^2 | x<-l , even x ])

{- e)
-}
primeraAparicion :: [Int] -> [(Int,Int)]
primeraAparicion l = []

{- f)
-}

{- g)
-}

{- h)
-}

{- i)
-}
contieneSublista :: [Int] -> [Int] -> Bool
contieneSublista l1 l2 = False -- no terminado

{- j)
-}
ordenar :: [Int] -> [Int]
ordenar l = l --no terminado

{- k)
-}

listaTerna :: [a] -> [a] -> [(a,a,a)]
listaTerna [] _ = []
listaTerna _ [] = []
listaTerna _ [x] = []
listaTerna (x:xs) (y1:y2:ys) =  (x,y1,y2):(listaTerna xs ys) 

{- l)
-}
alFinal :: a->[a]->[a]
alFinal a [] = a:[]
alFinal a (primero:resto) = primero: alFinal a resto  

{- m)
-}


{- n) -- no funciona bien
-}
reverseNoFinal :: [a] -> [a]
reverseNoFinal [] = [] 
reverseNoFinal (x:xs) = reverseNoFinal xs ++ [x]   


reverseFinal :: [a] -> [a]
reverseFinal (x:xs) = reverseFinalAux xs []  

reverseFinalAux :: [a] -> [a] -> [a]
reverseFinalAux [] acum  = acum
reverseFinalAux [x] acum  = acum++[x]
reverseFinalAux (x:xs) acum = reverseFinalAux xs acum++[x]

reverseFoldr :: [a] -> [a]
reverseFoldr l = foldr (\e acum -> acum++[e]) [] l  

{- o)
-}

reverseListaDeListas :: [[a]] -> [[a]]
reverseListaDeListas l = foldr (\e acum -> acum++[reverseFoldr e] ) [] l

{- p)
-}
-- sum 5 7
otroFlip :: (a->b->c)->b->a->c
otroFlip f p1 p2 = f p2 p1

{- q)
-}
mapPolimorfico :: (a->b)->[a]->[b] 
mapPolimorfico f l = [f(e) | e<-l]