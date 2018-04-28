import Data.List
import Data.Char
import Data.Map hiding (map, filter)

type CharCount = Map Char Int

wordCharCounts :: String -> CharCount
wordCharCounts cs = fromList $ zip nlower (count nlower)
  where
    lower  = map toLower cs
    nlower = nub lower
    count  = map (\c -> length (filter (==c) lower))


sentenceCharCounts :: String -> [CharCount]
sentenceCharCounts = map wordCharCounts . words


dictCharCounts :: [String] -> Map String CharCount
dictCharCounts ws = fromList $ zip ws (map wordCharCounts ws)


dictWordsByCharCounts :: Map String CharCount -> Map CharCount [String]
dictWordsByCharCounts m = fromListWith (++) p
  where
    p = [(cc, [w]) | (w, cc) <- toList m]
