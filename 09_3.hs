import Data.Char

upperOnly :: String -> String
upperOnly = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allUpper :: String -> String
allUpper [] = []
allUpper (x:xs) = toUpper x : allUpper xs

capitalizeHead :: String -> Char
--capitalizeHead [] = ' '
--capitalizeHead str = toUpper $ head str
capitalizeHead = toUpper . head
