import Data.List (intersperse)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops" -- "woops mrow " + XXX

frappe = flippy "haha" -- XXX ++ " mrow haha"

sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

multi :: (Integral a) => a -> a -> a
multi n 1 = n
multi n m = n + multi n (m - 1)

data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

dividedBy n d
  | d == 0 = DividedByZero
  | otherwise = Result (div n d)

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "?"

digits :: Int -> [Int]
digits n = [digitAt n i | i <- reverse [1 .. numDigits]]
  where
    numDigits = length $ show n
    digitAt :: Int -> Int -> Int
    digitAt num pos = num `mod` 10 ^ pos `div` 10 ^ (pos - 1)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))
