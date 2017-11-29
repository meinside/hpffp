dodgy :: Int -> Int -> Int
dodgy x y = x + y * 10

oneIsOne :: Int -> Int
oneIsOne = dodgy 1

oneIsTwo :: Int -> Int
oneIsTwo = flip dodgy 2
