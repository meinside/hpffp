import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello world"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

extractDate :: DatabaseItem -> UTCTime
extractDate (DbDate time) = time

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False

extractNumber :: DatabaseItem -> Integer
extractNumber (DbNumber number) = number

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map extractDate . filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map extractNumber . filter isDbNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = maximum filteredItems
  where
    filteredItems = filterDbDate items

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items =
  fromIntegral (sum filteredItems) / fromIntegral (length filteredItems)
  where
    filteredItems = filterDbNumber items
