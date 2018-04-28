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


wordAnagrams :: String -> Map CharCount [String] -> [String]
wordAnagrams w m = findWithDefault [] (wordCharCounts w) m


-- convert Map into List
-- split all counts which are >1 into 1s (expand) (('a', 2) -> ('a', 1), ('a', 1))
-- get all apossible subsets
-- remove duplicates
-- convert back all 1s by adding (('a', 1), ('a', 1) -> ('a', 2))
charCountsSubsets :: CharCount -> [[(Char, Int)]]
charCountsSubsets cc' = map toList $ map (fromListWith (+)) $ nub $ subsets $ expand $ toList cc'
  where
    expand []           = []
    expand ((c, 1):ccs) = (c, 1):(expand ccs)
    expand ((c, i):ccs) = (c, 1):(expand ((c, i-1):ccs))
    subsets []       = [[]]
    subsets (cc:ccs) = subsets ccs ++ map (cc:) (subsets ccs)
