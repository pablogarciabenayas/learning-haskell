module TypesAndData where

type Point = (Double, Double)

midpoint :: Point -> Point -> Point
midpoint (x1,x2) (y1,y2) = ((x1+x2)/2,(y1+y2)/2)

--Records NO USAR
{-
data CustomerId = MakeCustomerId{ 
identificator :: Int
}


data Customer = MakeCustomer{
 customerId :: CustomerId
 , name :: String
 , luckyNumber :: Int
 }
 
 
alice :: Customer
alice = MakeCustomer{
 customerId = MakeCustomerId 7
 ,name = "Alice"
 , luckyNumber = 10
}
-}

-- Tipos de datos algebraicoss
data Customer = Customer CustomerId String Int
-- nombreTipo = nombreConstructor TipoArgumento1 TipoArgumento2 TipoArgumento...
-- el nombre del constructor y el tipo se pueden llamar igual (se suele hacer asi)
data CustomerId = CustomerId Int

alice :: Customer
alice = Customer (CustomerId 13) "Alice" 42

getCustomerId :: Customer -> CustomerId
getCustomerId (Customer cust_id name luckyNumber) = cust_id

data StringTree = StringTree String [StringTree]

hierarchy = StringTree "c:" [StringTree "Program Files" [], StringTree "Users"
 [StringTree "Alice" []], StringTree "Cats" []]

-- Constructors de tipos de datos algebraicos
data DialogResponse = YES | No | Help | Quit
data MaybeInt = NoInt | JustInt Int

defaultInt :: Int ->MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x) = x


data StringList = EmptyStringList
 | ConsStringList String StringList
 

--Type clases
data RGB = RGB Int Int Int

instance Eq RGB where
 (RGB r1 g1 b1) == (RGB r2 g2 b2) = 
  (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Show RGB where 
 show (RGB r g b) = "RGB "++(show r) ++ " " ++ (show g) ++ " "++ (show b)
 
 
