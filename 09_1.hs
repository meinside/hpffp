myEnumFromTo :: (Ord a, Enum a) => a -> a -> [a]
myEnumFromTo from to
  | from > to = []
  | from == to = [from] -- without this guard, `(succ from)` below fails with bounded values like `True`
  | otherwise = from : myEnumFromTo (succ from) to

myWords :: String -> [String]
myWords "" = []
myWords str = word : rest
  where
    word = takeWhile (/= ' ') str
    rest = myWords $ dropWhile (== ' ') (dropWhile (/= ' ') str)

myLines :: String -> [String]
myLines "" = []
myLines str = line : rest
  where
    line = takeWhile (/= '\n') str
    rest = myLines $ dropWhile (== '\n') (dropWhile (/= '\n') str)

split :: String -> Char -> [String]
split "" _ = []
split str token = word : rest
  where
    word = takeWhile (/= token) str
    rest = split (dropWhile (== token) (dropWhile (/= token) str)) token
