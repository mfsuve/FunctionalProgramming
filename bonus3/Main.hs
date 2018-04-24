import Data.List
import Data.Char

type CharCount = [(Char, Int)]

wordCharCounts :: String -> CharCount
wordCharCounts cs = zip nlower (count nlower)
  where
    lower  = map toLower cs
    nlower = nub lower
    count  = map (\c -> length (filter (==c) lower))