import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
type Word = String
data Action = Add [String] | Search String | Find String | Print | Exit
    deriving Show

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


getInput :: IO (String, [String])
getInput = do
            putStrLn "a) Add Word"
            putStrLn "s) Search Word"
            putStrLn "f) Find words with prefix"
            putStrLn "p) Print all words"
            putStrLn "e) Exit"
            putStrLn "Enter the action:"
            action <- getLine
            if action == "p" || action == "e" then
                return (action, [])
                else do
                    putStrLn "Enter word/prefix:"
                    info <- getLine
                    return (action, words info)


convertAction :: (String, [String]) -> Maybe Action
convertAction a = case a of
    ("a", ss)  -> Just $ Add ss
    ("s", [s]) -> Just $ Search s
    ("f", [s]) -> Just $ Find s
    ("p", _)   -> Just $ Print
    ("e", _)   -> Just $ Exit
    _          -> Nothing


main = do
        input <- getInput
        let action = convertAction input
        putStrLn $ show action
