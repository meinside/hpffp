myOr :: [Bool] -> Bool
myOr [] = False
--myOr (x:xs) =
--  if x
--    then True
--    else myOr xs
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = any (e ==) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish = concat

squish' :: [[a]] -> [a]
squish' xs = foldl (++) [] xs

squish'' :: [[a]] -> [a]
squish'' xs = concat xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl myMax x xs
  where
    myMax a b =
      case f a b of
        GT -> a
        _ -> b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl myMin x xs
  where
    myMin a b =
      case f a b of
        LT -> a
        _ -> b
