allWords :: String -> String -> [(Char, Char, Char)]
allWords stops vowels = [(c1, c2, c3) | c1 <- stops, c2 <- vowels, c3 <- stops]

allWordsBeginWithP :: [(Char, Char, Char)]
allWordsBeginWithP
  --[(c1, c2, c3) | (c1, c2, c3) <- allWords "pbtdkg" "aeiou", c1 == 'p']
 = [('p', c2, c3) | c2 <- "aeiou", c3 <- "pbtdkg"]

sentences :: [String] -> [String] -> [(String, String, String)]
sentences nouns verbs = [(w1, w2, w3) | w1 <- nouns, w2 <- verbs, w3 <- nouns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' :: String -> Double
seekritFunc' x =
  (/)
    (fromIntegral (sum (map length (words x))))
    (fromIntegral (length (words x)))
