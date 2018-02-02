module TestModule where

test :: Int -> Int
test n = 2 * n

--DefinicionLocal
--circunferencia :: Float -> (Float , Float)
--circunferencia r = (2 * p * r , p * r * r ) where
-- p = 3.141592
 
 --Recibe una lista de numeros enteros y un numero, devuelve los que son pares 3 casos:
 --Recursividad pura
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
  
  
--Funcion ternas, recibe dos listas con numeros enteros y devuelve una lista con tuplas que contenga un elemento de la primera lista y dos de la segunda
terna:: [Int]->[Int]->[(Int,Int,Int)]
terna [] _ = []
terna _ [] = []
terna _ [x] = []
terna (x:xs) (y1:y2:ys) =  (x,y1,y2):(terna xs ys)


----separar lista, recibe una lista de enteros y devuelve dos listas, en la priemra lista aparecen los valores no repetidos y en la segunda los valores repetidos
--separar::([Int] -> [Int] -> [Int])
--separar l = separarAux [] []

--separarAux:: [Int] -> [Int]->[Int]->([Int], [Int])
--separarAux (n:ns) acum1 acum2 = if contiene n acum2 then 
--   separarAux ns acum1 acum2 
--  else contiene n acum 1 
--   separarAux ns (borrar n acum1) (acum2++[n])
--  else 
--   separarAux ns (acum1++[n]) acum2
 

--contiene:: Int -> [Int] -> Bool
--contiene _ [] = false
--contiene x (n:ns) = (x==n) || (contiene x ns)

borrar:: Int -> [Int] -> [Int]
borrar _ [] = []
borrar x (n:ns) = if (x==n) then borrar x ns else n:(borrar x ns)


--Función que devuelva cuantas secuencias de 0 hay en una lista
-- Dada una lista de numeros enteros, devolver una lista de listas, donde lo que contiene cada sublista, son los numeros que hay entre los ceros
ceros::[Int]->Int
ceros [] = 0
ceros [x] = if x==0 then 1 else 0
ceros (x:y:ys) = if ((x==0) && (y /= 0)) then 1 + ceros ys 
 else ceros (y:ys)
 
ceros' :: [Int] -> [[Int]]
ceros' l = ceros'' l [][]


ceros'' :: [Int]->[Int]->[[Int]]->[[Int]]
ceros'' [] acum1 acum2 = acum2++[acum1]
ceros'' (n:ns) acum1 acum2 = if (n==0) then ceros'' ns [] (acum2++[acum1]) 
 else ceros'' ns (acum1++[n]) acum2
 

-- foldl(\acum e -> acum++[e*2])

--Función que recibe un elemento y lo añade al final de la lista
final:: [Int]->Int->[Int]
final l n = foldr(\e acum ->e:acum) [n] l
 
 
--Función que recibe una lista de duplas y queremos que devuelva la lista de tuplas pero dando la vuelta a los elemenos
-- ejemplos [(7,1),(4,3),(5,0)] y devuelve [(1,7),(3,4),(0,5)]
darVuelta::[(Int,Int)]->[(Int,Int)] 
darVuelta l = foldr(\(x,y) acum ->(y,x):acum ) [] l


--Función que recibe un elemento y una lista y elimina todas la apareciones de ese elemento
eliminarElemento::[Int]->Int->[Int]
eliminarElemento l e = foldl(\acum x -> if(x==e) then acum else acum++[x]) [] l


--Función cribar con función foldl
cribarFoldr::[Int]->Int->[Int]
cribarFoldr l n = foldr(\e acum -> if (e`mod` n==0) then acum else e:acum) [] l

--Función que recibe un string y devuelve una dupla que sea (vocales,consonantes)
separar::[Char]->([Char],[Char])
separar l = foldl(\ (v,c) letra-> if esVocal letra then (v++[letra],c) else (v,c++[letra])) ([],[]) l

esVocal::Char->Bool
esVocal 'a'= True
esVocal 'e' = True
esVocal 'i'= True
esVocal 'o'= True
esVocal 'u'= True
esVocal 'A'= True
esVocal 'E' = True
esVocal 'I'= True
esVocal 'O'= True
esVocal 'U'= True
esVocal _ = False

--Función que recibe una lista de números enteros y dos numeros, y sustituye el primer número paror el segundo cuando este en una posición par de la lista
sustituir::(Eq a)=>[a]->a->a->[a]
sustituir l x y = foldl (\acum n-> if (n==x) && (even (length acum+1)) then acum++[y] else acum++[n]) [] l

--Función que recibe una lista de numeros enteros con foldr y separe en una lista los numeros pares y en otra los impares
separarPares::[Int]->([Int],[Int])
separarPares l = foldr (\n (p,i) -> if even n then (n:p,i) else (p,n:i)) ([],[]) l


--Ordenar una lista utilizando el mergesort
mergesort:: [Int]->[Int]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort a) (mergesort b)
 where (a,b) = split l [] (length l`div`2)

merge:: [Int]->[Int]->[Int]
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = if (x < y) then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

split::[Int]->[Int]->Int->([Int],[Int])
split (x:xs) acum l = if length acum == 1 then (acum ,(x:xs)) else split xs (acum++[x]) l


--Función que recibe una lista y devuelve una lista de listas con la secuencia de creación de esa lista
secuencia::[Int]->[[Int]]
secuencia l = foldl (\acum n -> acum++[last acum ++[n]]) [[]] l

--Mismo que el anterior pero sin el uso de foldl
secuencia1::[Int]->[[Int]]
secuencia1 []=[[]]
secuencia1 l = secuencia1 (init l)++[l]


--Funcion subconjuntos posibles
--Función que coja los n mayores numeros de una lista
