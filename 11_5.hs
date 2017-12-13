import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:xs) (y:ys) =
  if x == y
    then isSubsequenceOf xs ys
    else isSubsequenceOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = [(w, capitalize w) | w <- words str]
  where
    capitalize :: String -> String
    capitalize [] = []
    capitalize (s:ss) = toUpper s : ss

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (s:ss) = toUpper s : ss

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn a xs =
  [s | s <- takeWhile (\c -> c /= a) xs] :
  splitOn a [t | t <- tail $ dropWhile (\c -> c /= a) xs]

capitalizeParagraph :: String -> String
capitalizeParagraph str = unwords $ foldl f [] $ words str
  where
    f :: [String] -> String -> [String]
    f [] s = [capitalizeWord s]
    f ss s =
      if last (last ss) == '.' -- if previous word ends with '.',
        then ss ++ [capitalizeWord s]
        else ss ++ [s]
