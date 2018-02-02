
module Libreria where


{- 
Se quiere desarrollar un sistema para dar soporte a un repositorio de librerías de un lenguaje de
programación. Una librería en este lenguaje de programación se puede identificar por un nombre
(representado como una cadena de caracteres) y una versión. Las versiones a su vez están
compuestas de dos números: major version y minor version. Textualmente se representan
separados por un punto: 5.1. El major version es el número a la izquierda del punto, el minor
version es el número a la derecha.
El sistema almacena las librerías físicamente en disco y lleva un control de qué librerías hay en
el repositorio. Para ello se desea poder tener las librerías ordenadas. Se pide:
Implementar las clases necesarias para poder mantener ordenadas las librerías. En el caso de
dos librerías con nombres diferentes, deben ordenarse por orden alfabético del nombre. En el
caso de dos versiones de la misma librería, deben ordenarse por el número de versión.
Aparecerá antes la librería con un número de major version más bajo, o si tienen igual major
version, la que tenga el minor version más bajo. Por ejemplo:
Orden correcto:
docker 2.1, docker 2.2, docker 3,2, mtt 3.3
ansible 1.2, ansible 1.21, ruby 2.0
Orden incorrecto:
docker 2.1, docker 1.3
mtt 3.1, docker 2.1
Implementar una clase especial Compatible con una función que permita determinar si dos
versiones de la misma librería son compatibles entre sí. Dos versiones de la misma librería son
compatibles si su major version coincide y sólo difieren en el minor version. Por ejemplo:
docker 2.1 y docker 2.3 serían compatibles
doker 2.1 y docker 3.2 no serían compatibles
docker 2.1 y mtt 2.3 no serían compatibles porque no es la misma librería
Implementar una función que dada una lista de librerías y una librería devuelva todas las librerías
de la lista que son compatibles con la librería dada.

̣-}



data Libreria = Libreria String Int Int 

instance Eq Libreria where
 (Libreria nombre1 major1 minor1) == (Libreria nombre2 major2 minor2) =
  nombre1 == nombre2

instance Show Libreria where
 show (Libreria  nombre major minor) = 
  (show nombre)++ " " ++(show major)++"."++(show minor)
  
instance Ord Libreria where
 (Libreria n1 maj1 min1) > (Libreria n2 maj2 min2) = if n1 == n2 then maj1 > maj2 else n1 > n2
 (Libreria n1 maj1 min1) >= (Libreria n2 maj2 min2) = if n1 == n2 then maj1 >= maj2 else n1 >= n2
 (Libreria n1 maj1 min1) < (Libreria n2 maj2 min2) = if n1 == n2 then maj1 < maj2 else n1 < n2
 (Libreria n1 maj1 min1) <= (Libreria n2 maj2 min2) = if n1 == n2 then maj1 <= maj2 else n1 <= n2

class Compatible l where
 sonCompatibles:: l -> l -> Bool

instance Compatible Libreria where
 sonCompatibles (Libreria n1 maj1 min1) (Libreria n2 maj2 min2) = 
  if (Libreria n1 maj1 min1) == (Libreria n2 maj2 min2) then
   maj1 == maj2 && min1 /= min2 else False
  


listarCompatibles:: [Libreria]->Libreria->[Libreria]
listarCompatibles l lib =
 foldr(\x acum -> if (sonCompatibles x lib) then [x]++acum else acum ) [] l

 
-- prueba
-- listarCompatibles [(Libreria "x" 1 3),(Libreria "x" 1 4),(Libreria "x" 3 2),(Libreria "x" 1 2)] (Libreria "x" 1 2)