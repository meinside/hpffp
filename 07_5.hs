tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where
    (xLast2, _) = x `divMod` 100
    (_, d2) = xLast2 `divMod` 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ True = x
foldBool3 _ y False = y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b
  | b = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b =
  if b
    then x
    else y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)
