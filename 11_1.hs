data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data PlaneSize
  = Small
  | Medium
  | Large
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          PlaneSize
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Plane _ _) = False
isCar _ = True

isPlane :: Vehicle -> Bool
--isPlane a = not (isCar a)
isPlane (Car _ _) = False
isPlane _ = True

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
