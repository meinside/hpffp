{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

newtype Something =
  Something (Int, String)
  deriving (Eq, Show)

instance TooMany Something where
  tooMany (Something (n, _)) = n > 42

newtype AnotherThing =
  AnotherThing (Int, Int)
  deriving (Eq, Show)

instance TooMany AnotherThing where
  tooMany (AnotherThing (m, n)) = m + n > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (m, n) = tooMany (m + n)
