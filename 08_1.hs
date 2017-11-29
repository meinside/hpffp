applyTimes :: (Eq a, Num a) => a -> (a -> a) -> a -> a
applyTimes 0 _ x = x
applyTimes n f x = f (applyTimes (n - 1) f x)
