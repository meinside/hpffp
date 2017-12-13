import Data.Char

data DaPhone =
  DaPhone [KeyMap]
  deriving (Show)

data KeyMap =
  KeyMap Digit
         [Char]
  deriving (Show)

daPhone :: DaPhone
daPhone =
  DaPhone
    [ KeyMap '1' []
    , KeyMap '2' ['a', 'b', 'c']
    , KeyMap '3' ['d', 'e', 'f']
    , KeyMap '4' ['g', 'h', 'i']
    , KeyMap '5' ['j', 'k', 'l']
    , KeyMap '6' ['m', 'n', 'o']
    , KeyMap '7' ['p', 'q', 'r', 's']
    , KeyMap '8' ['t', 'u', 'v']
    , KeyMap '9' ['w', 'x', 'y', 'z']
    , KeyMap '*' ['^']
    , KeyMap '0' [' ', '+', '_']
    , KeyMap '#' ['.', ',']
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone keymaps) c
  | isUpper c = searchChar keymaps '^' : reverseTaps phone (toLower c)
  | isNumber c || c == '*' || c == '#' =
    let (KeyMap digit characters) = head $ filter fn keymaps
          where
            fn (KeyMap dgt _) = dgt == c
    in [(digit, length characters + 1)]
  | otherwise = [searchChar keymaps c]
  where
    searchChar :: [KeyMap] -> Char -> (Digit, Presses)
    searchChar keymaps' ch = (digit, (findIndex ch chars) + 1)
      where
        (KeyMap digit chars) = searchKeyMap keymaps' ch
          where
            searchKeyMap :: [KeyMap] -> Char -> KeyMap
            searchKeyMap kms ch' = head $ filter f kms
              where
                f (KeyMap _ chars') = ch' `elem` chars'
        findIndex :: Char -> [Char] -> Int
        findIndex _ [] = -999999
        findIndex c' (c'':cs) =
          if c' == c''
            then 0
            else 1 + findIndex c' cs

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone str = concat $ map (reverseTaps phone) str

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps taps = foldr (+) 0 (flatten taps)
  where
    flatten :: [(Digit, Presses)] -> [Presses]
    flatten taps' = map snd taps'

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort [l | l <- xs, l < x] ++ [x] ++ sort [r | r <- xs, r >= x]

labelCounts :: (Ord a, Eq a) => [a] -> [(a, Int)]
labelCounts elms = countElement [] (sort elms)
  where
    countElement :: (Ord a, Eq a) => [(a, Int)] -> [a] -> [(a, Int)]
    countElement counts [] = counts
    countElement [] (x:xs) = countElement [(x, 1)] xs
    countElement counts (x:xs) =
      if lastElement == x
        then countElement (others ++ [(x, lastElementCount + 1)]) xs
        else countElement (counts ++ [(x, 1)]) xs
      where
        lastCount = last counts
        lastElement = fst lastCount
        lastElementCount = snd lastCount
        others = init counts

count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count (x:xs) a =
  if x == a
    then 1 + (count xs a)
    else count xs a

mostPopularLetter :: String -> Char
mostPopularLetter str = fst mostPopularPair
  where
    charCounts = labelCounts str
    mostPopularPair = maxChar (tail charCounts) (head charCounts)
    maxChar :: [(Char, Int)] -> (Char, Int) -> (Char, Int)
    maxChar [] mx = mx
    maxChar (x:xs) (mxChar, mxCount) =
      if snd x > mxCount
        then maxChar xs (fst x, snd x)
        else maxChar xs (mxChar, mxCount)

mostPopularLettersCost :: String -> Presses
mostPopularLettersCost str =
  (fingerTaps $ reverseTaps daPhone mostPopular) * numMostPopular
  where
    mostPopular = mostPopularLetter str
    numMostPopular = count str mostPopular

coolestLtr :: [String] -> Char
coolestLtr strs = mostPopularLetter $ concat strs

coolestWord :: [String] -> String
coolestWord strs = fst (topWord wordCounts)
  where
    wordCounts = labelCounts $ concat $ map words strs
    topWord :: [(String, Int)] -> (String, Int)
    topWord labeledCounts = topWord' (head labeledCounts) (tail labeledCounts)
      where
        topWord' :: (String, Int) -> [(String, Int)] -> (String, Int)
        topWord' mxWord [] = mxWord
        topWord' mxWord (x:xs) =
          if snd mxWord < snd x
            then topWord' x xs
            else topWord' mxWord xs
