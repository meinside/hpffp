module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar str offset = map f str
  where
    f c = chr (ord c + offset)

unCaesar :: String -> Int -> String
unCaesar str offset = map f str
  where
    f c = chr (ord c - offset)
