divideBy :: (Integral a) => a -> a -> (a, a)
divideBy number devider = divide devider (0, number)
  where
    divide :: (Integral a) => a -> (a, a) -> (a, a)
    divide d (quotient, remainder)
      | d > remainder = (quotient, remainder)
      | otherwise = divide d (quotient + 1, remainder - d)
