
module Examen.Examen where

-- Examen Pablo García Benayas

type Acronimo = String
type NombreCompleto = [String]
data ConceptoMedico = ConceptoMedico Acronimo | NombreCompleto deriving Show

instance Eq ConceptoMedico where 
 (ConceptoMedico a1) == (ConceptoMedico a2) = a1 == a2
    

insertarConcepto :: ConceptoMedico ->[ConceptoMedico]->[ConceptoMedico]
insertarConcepto c [] = c:[]
insertar c l = if existeConcepto c l then c:l else l  


existeConcepto :: ConceptoMedico -> [ConceptoMedico] -> Bool
existeConcepto c [] = False
existeConcepto c [x] = c==x
existeConceto c (x:xs) = existeConceto c xs

sustituir :: ConceptoMedico -> [ConceptoMedico] -> [ConceptoMedico] 
sustituir c l = foldl(\x acum -> ) [] l