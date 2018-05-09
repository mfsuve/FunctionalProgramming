import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
data Action = Add (Either String [String]) | Search String | Find String | Print
type Word = String

empty :: Trie
empty = undefined

insert :: Word -> Trie -> Trie
insert = undefined

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined


getInput :: IO (String, String)
getInput = do
            putStrLn "a) Add Word"
            putStrLn "s) Search Word"
            putStrLn "f) Find words with prefix"
            putStrLn "p) Print all words"
            putStrLn "e) Exit"
            putStrLn "Enter the action:"
            action <- getLine
            if action == "p" || action == "e" then
                return (action, "")
                else do
                    putStrLn "Enter word/prefix:"
                    info <- getLine
                    return (action, info)
