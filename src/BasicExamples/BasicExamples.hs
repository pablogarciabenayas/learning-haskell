
module BasicExamples where


{-
 :reload -> recarga el modulo en el interprete
 :set + t -> indica el tipo de salida
-}
celsius:: Integer -> Integer
celsius f = (f-32)*5`div`9

sucesor :: Integer ->Integer
sucesor (x) = x +1 

--Composición de funciones
doble :: Integer -> Integer
doble x = x + x

cuadruple :: Integer -> Integer
cuadruple (x) = doble(doble(x))

-- fin de composicion de funciones


divReal :: (Integer, Integer) -> Double
divReal (x,y) =fromInteger (x) / fromInteger (y)

parteEnteraDecimal :: Double -> (Integer,Double)
parteEnteraDecimal x = (truncate x, x - fromInteger(truncate x))


-- expresiones condicionales
-- la rama else es siempre obligatoria!!!


--Recursividad
potencia :: (Integer,Integer) -> Integer
potencia (b,e) = if e == 0 then 1
 else b * potencia(b,e-1)
 
--Recursivdad con parametros de acumulación
factorial :: Integer -> Integer
factorial x = factorial2(x,1)

factorial2 :: (Integer, Integer) -> Integer
factorial2 (n,r) = if n==0 then r
 else factorial2(n-1,n*r)
 
{-
apariciones


coincide -}


-- Listas
longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 

-- polimorfismo
longitudPolimorfica :: [a] -> Integer
longitudPolimorfica [] = 0
longitudPolimorfica (x:xs) = 1 + longitudPolimorfica xs

{-- este caso da error porque no el operador + no está disponible
para la suma de string por ejemplo. 
suma :: [a] -> a
suma [] = 0
suma (x:xs) = x + suma(xs)

Se puede solucionar limitando que tipos pueden ser polimorficos
--}

-- En este caso suma solo puede ser llamada por un tipo que derive
-- de num
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma(xs)


--Añadir un elemento al final de una lista
alFinal :: (a,[a])->[a]
alFinal (e,[]) = e:[]
alFinal (e,(l:ls)) = l : alFinal(e,ls) 


-- Se pide una funcion que dada una lista de enteros devuelva una 
-- lista de dos-tupla, donde para cada entero se muestre el entero y el indice
-- de la primera aparicion en la lista
-- [1,3,4,4,6,8,1] => [(1,1),(3,2),(4,3),(6,5),(8,6)]


primeraAparicion :: [Integer] -> [(Integer,Integer)]
primeraAparicion l = primAux(l,1,[],[])

primAux :: ([Integer],Integer,[Integer],[(Integer,Integer)]) -> [(Integer,Integer)]
primAux ([],pos, vistos, resultado) = resultado
primAux ((primero:resto),  pos, vistos, resultado) = 
 if esta(primero,vistos) then
   primAux(resto, pos + 1,vistos, resultado)
 else
   primAux(resto, pos + 1, (primero:vistos),(primero,pos):resultado)

esta :: (Integer,[Integer]) -> Bool
esta (e, []) = False
esta (e, primero:resto) = if e == primero then True
 else esta(e, resto)   
 

--Listas por comprensión

--Generadores

--Filtros


-- Tipos de datos definidos por el usuario
type Coordenadas = (Integer, Integer)
transPunto :: Coordenadas -> Coordenadas
transPunto (x,y) = (y,x)

data Palo = Oros | Copas | Espadas | Bastos 
 deriving (Eq, Show)

igualPalo :: (Palo,Palo) -> Bool
igualPalo(x,y) = x == y

data Figura = AS | Dos | Tres | Cuatro | Cinco | Seis | Siete |
 Sota | Caballo | Rey

data Carta = Carta(Figura,Palo)

damePalo:: Carta -> Palo
damePalo (Carta(figura,palo)) = palo

data Poligono = Triangulo(Double, Double)
 | Cuadrado Double
 | Rectangulo (Double,Double)

area::Poligono -> Double
area (Triangulo(b,h)) = (1/2) * b *h
area (Cuadrado lado) = lado * lado
area (Rectangulo (b,h)) = b * h


--Definiciones locales
 --con let
raicesLet:: (Double,Double,Double) -> (Double,Double)
raicesLet(a,b,c) = 
 let raizDisc = sqrt(b^2 - 4*a*c); denom = 2*a 
  in ((b+raizDisc) / denom, (b-raizDisc / denom))
     
raicesWhere::(Double,Double,Double) -> (Double,Double)
raicesWhere(a,b,c) = ((b + raizDisc) / denom, (b-raizDisc/denom))
 where
  raizDisc = sqrt(b^2-4*a*c)
  denom = 2 *a
 
 
--Clases
