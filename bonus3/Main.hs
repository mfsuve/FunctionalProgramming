import Prelude hiding (lookup)
import Data.List as List hiding (lookup)
import Data.Char
import Data.Map as Map hiding (map, filter, foldr)
import Data.Maybe
import System.IO
import System.Environment
import Debug.Trace

type CharCount = Map Char Int

-- Example: wordCharCounts "ate"
-- Example: wordCharCounts "love"
-- Returns: Character count of the given word
-- Input string assumed to have no spaces. It is only a word
-- Without punctuation
wordCharCounts :: String -> CharCount
wordCharCounts cs = fromList $ zip nlower (count nlower)
  where
    lower  = map toLower cs
    nlower = nub lower
    count  = map (\c -> length (filter (==c) lower))


-- Example: sentenceCharCounts "Mickey Mouse"
-- Example: sentenceCharCounts "i love you"
-- Returns: Character count of the given sentence
-- Input string can have spaces in it since it is treated as a sentence
-- Without punctuation
sentenceCharCounts :: String -> CharCount
sentenceCharCounts s = wordCharCounts $ foldr (++) [] $ words s


-- Example: readDict
-- Returns: List containing all the words that are in the dictionary
-- This function does not take any parameter
readDict :: IO [String]
readDict = do
            file <- openFile "words.txt" ReadMode
            contents <- hGetContents file
            return (words contents)


-- Example: dictCharCounts ["ate", "eat", "tea", "love"]
-- Returns: Mapping from words in the list to their character counts
-- this function is used to get the character counts of all the words in the dictionary
dictCharCounts :: [String] -> Map String CharCount
dictCharCounts ws = fromList $ zip ws (map wordCharCounts ws)


-- Example: dictWordsByCharCounts $ dictCharCounts $ ["ate", "eat", "tea", "love"]
-- Returns: Mapping from all the possible char counts to words in the dictionary having that char count
-- this function is used with dictCharCounts function, taking the result of its output as input
dictWordsByCharCounts :: Map String CharCount -> Map CharCount [String]
dictWordsByCharCounts m = fromListWith (++) p
  where
    p = [(cc, [w]) | (w, cc) <- toList m]


-- Example: wordAnagrams "ate" $ dictWordsByCharCounts $ dictCharCounts $ ["ate", "eat", "tea", "love"]
-- Returns: anagrams of the word w that are in the dictionary
-- this function is used with dictCharCounts and dictWordsByCharCounts functions, taking the result of their outputs as input
wordAnagrams :: String -> Map CharCount [String] -> [String]
wordAnagrams w m = findWithDefault [] (wordCharCounts w) m


-- Example: charCountsSubsets $ wordCharCounts "ate"
-- Returns: the subset of the given character count
-- Explanations about how it works:
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


-- Example: subtractCounts (wordCharCounts "ate") (wordCharCounts "at")
-- Returns: Character count resulted from subtracting all the counts of the second input
--          from the counts of the first input
subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts = differenceWith (\c1 c2 -> if c1==c2 then Nothing else Just (c1-c2))


-- Example: sentenceAnagrams "i love you"
-- Returns: String list containing the anagrams of the given sentence
sentenceAnagrams :: String -> IO [String]
sentenceAnagrams s = do
              dict <- readDict
              return (anagrams s dict)


-- Example: anagrams "i love you" ["ate", "eat", "tea", "love"]
-- Returns: String list containing the anagrams of the given sentence
-- this function is used with sentenceAnagrams function, taking the dictionary from it
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


main = do
    args <- getArgs
    let s = args !! 0
    result <- sentenceAnagrams s
    let lined = if List.null result then "No anagrams" else foldr1 (\s1 s2 -> s1 ++ "\n" ++ s2) result
    putStrLn lined
