fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

fibs' :: [Int]
fibs' = take 20 fibs

fibs'' :: [Int]
fibs'' = filter (< 100) fibs

factorial :: [Int]
factorial = scanl (*) 1 (enumFrom 2)
