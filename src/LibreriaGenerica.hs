
module LibreriaGenerica where




type Nombre = String
type Iva = Double 
type PrecioBase = Double  

data Tipo = Tipo Nombre Iva
data Producto = Producto Tipo PrecioBase  

pvp:: Producto -> Double
pvp (Producto(Tipo nombre iva) base) = base + (base*iva)/100

precioBase :: Producto -> Double
precioBase (Producto(Tipo nombre iva) base) = base

iva:: Producto -> Double
iva (Producto(Tipo nombre iva) base) = iva

tasa:: Producto ->Double
tasa (Producto(Tipo nombre iva) base) = (base*iva)/100

