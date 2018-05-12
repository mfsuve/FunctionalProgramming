import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
    deriving Show
type Word = String
data Action = Add [String] | Search String | Find String | Print | Exit
    deriving Show


empty :: Trie
empty = Trie {end=False, children=M.empty}


insert :: Word -> Trie -> Trie
insert []     Trie{children=chl} = Trie{end=True,  children=chl}
insert (c:cs) Trie{children=chl} = Trie{end=False, children=toMap False (M.toList chl)}
        where
            toMap found []
                | found     = M.empty
                | otherwise = M.insert c (insert cs empty) M.empty
            toMap found ((c', t):xs)
                | c==c'     = M.insert c (insert cs t) (toMap True xs)
                | otherwise = M.insert c' t (toMap found xs)


insertList :: [Word] -> Trie
insertList = foldr insert empty


search :: Word -> Trie -> Bool
search [] Trie{end=e} = e
search w Trie{end=e, children=chl} = find w (M.toList chl)
    where
        find _ []       = False
        find w'@(c:cs) ((c', t):chls)
            | c==c'     = search cs t
            | otherwise = find w' chls


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
