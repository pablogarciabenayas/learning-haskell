
module Hoja3 where

{- a)
-}

-- type
type Numerador = Integer
type Denominador = Integer
equivalentes :: [(Numerador,Denominador)] -> (Numerador,Denominador) -> [(Numerador,Denominador)]
equivalentes [] _ = []
equivalentes [(n,d)] (num,denom) = [(n,d) | (n,d) <- [(n,d)],  n * denom == d * num]


-- data
data Racional = Numerador Denominador deriving (Show)
equivalentesData :: [Racional] -> Racional -> [Racional]
equivalentesData l r = []

{- b)
-}

{- c)
-}
data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
data Tipo = Laborable | NoLaborable deriving (Show)

esLaborable :: Dia -> Tipo
esLaborable Sabado = NoLaborable
esLaborable Domingo = NoLaborable
esLaborable _ = Laborable
 

{- d)
-}

{- e)
-}

--data Euro = String Double
--data Dollar = String Double 

{- f)
-}

--data Expr = Valor Integer
--	|Expr :+: Expr
--	|Expr :-: Expr
--	|Expr :*: Expr
--	
-- función que calcula el valor de una expresión

-- función que calcula el número de constantes de una expresión