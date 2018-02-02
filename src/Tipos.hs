module Tipos where

data Temperatura = Frio | Caliente deriving Show
data Estacion = Primavera | Verano | Otonyo | Invierno deriving Show

tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente
tiempo Verano = Caliente
tiempo _ = Frio