import Prelude hiding (lookup)
import Data.List as List hiding (lookup)
import Data.Char
import Data.Map as Map hiding (map, filter, foldr)
import Data.Maybe
import System.IO
import System.Environment
import Debug.Trace

type CharCount = Map Char Int

wordCharCounts :: String -> CharCount
wordCharCounts cs = fromList $ zip nlower (count nlower)
  where
    lower  = map toLower cs
    nlower = nub lower
    count  = map (\c -> length (filter (==c) lower))


sentenceCharCounts :: String -> CharCount
sentenceCharCounts s = wordCharCounts $ foldr (++) [] $ words s


readDict :: IO [String]
readDict = do
            file <- openFile "words.txt" ReadMode
            contents <- hGetContents file
            return (words contents)


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
charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map (fromListWith (+)) . nub . subsets . expand . toList
  where
    expand []           = []
    expand ((c, 1):ccs) = (c, 1):(expand ccs)
    expand ((c, i):ccs) = (c, 1):(expand ((c, i-1):ccs))
    subsets []       = [[]]
    subsets (cc:ccs) = subsets ccs ++ map (cc:) (subsets ccs)


subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts = differenceWith (\c1 c2 -> if c1==c2 then Nothing else Just (c1-c2))


sentenceAnagrams :: String -> IO [String]
sentenceAnagrams s = do
              dict <- readDict
              return (anagrams s dict)


anagrams :: String -> [String] -> [String]
anagrams s dict = foldr (++) [] [subsetAnagrams ss cc | ss <- charCountsSubsets cc]
  where
    cc = sentenceCharCounts s

    subsetAnagrams :: CharCount -> CharCount -> [String]
    subsetAnagrams ss cc
      | Map.null remains = wordsFrom cc
      | otherwise        = [w ++ " " ++ rest | w    <- wordsFrom ss,
                                               ss'  <- charCountsSubsets remains,
                                               rest <- subsetAnagrams ss' remains]
        where
          remains = subtractCounts cc ss

    charCountToWords = dictWordsByCharCounts $ dictCharCounts $ dict

    wordsFrom :: CharCount -> [String]
    wordsFrom cc'' = [w | w <- fromMaybe [] (lookup cc'' charCountToWords)]
