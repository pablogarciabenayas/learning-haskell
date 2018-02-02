
module Hoja4 where

{- a)
-}

{- b)
-}

divisiones :: Fractional a => a->[a]->[a]
divisiones n l = [n/x|x<-l]

{- c)
-}

data Arbol a = AV | Rama (Arbol a) a (Arbol a)


{- d)
-}
--data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

{- e)
-}
type Nombre = String

data Titulacion = GradoII | GradoII_ADE | Grado_ADE
data Estudiante = Nombre Titulacion

{- f)
-}

{- g)
-}

{- h)
-}

{- i)
-}