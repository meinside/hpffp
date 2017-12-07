myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (a ==)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (a ==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr fn []
  where
    fn x ys =
      if f x
        then x : ys
        else ys

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl fn x xs
  where
    fn v1 v2 =
      case f v1 v2 of
        GT -> v1
        _ -> v2

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl fn x xs
  where
    fn v1 v2 =
      case f v1 v2 of
        LT -> v1
        _ -> v2
