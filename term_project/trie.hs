import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
    deriving Show
type Word = String
data Action = Add String | Search String | Find String | Print | Exit
    deriving Show


empty :: Trie
empty = Trie {end=False, children=M.empty}


insert :: Word -> Trie -> Trie
insert []     Trie{children=chl}        = Trie{end=True,  children=chl}
insert (c:cs) Trie{end=e, children=chl} = Trie{end=e, children=toMap False (M.toList chl)}
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
getWords Trie{end=e, children=chl} = allWords e [] (M.toList chl)
    where
        allWords e' acc []                                       = if e' then [acc] else []
        allWords e' acc ((c, Trie{end=e'', children=chl'}):chls) = allWords e'' (acc ++ [c]) (M.toList chl') ++ allWords e' acc chls


prefix :: Word -> Trie -> Maybe [Word]
prefix w = remain w
    where
        remain ""     t                  = Just $ (map (w++) . getWords) t
        remain (c:cs) Trie{children=chl} = case (M.lookup c chl) of
            Nothing -> Nothing
            Just t  -> remain cs t


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
                return (action, [])
                else do
                    putStrLn "Enter word/prefix:"
                    info <- getLine
                    return (action, info)


convertAction :: (String, String) -> Maybe Action
convertAction a = case a of
    ("a", s)   -> Just $ Add s
    ("s", s)   -> Just $ Search s
    ("f", s)   -> Just $ Find s
    ("p", _)   -> Just $ Print
    ("e", _)   -> Just $ Exit
    _          -> Nothing


doAction :: Maybe Action -> Trie -> IO ()
doAction Nothing t = do
                    putStrLn "Wrong Input"
                    loop t
doAction (Just (Add w)) t  = loop (insert w t)
doAction (Just (Search w)) t
    | search w t = do putStrLn "Exists in dictionary!"
                      loop t
    | otherwise  = do putStrLn "NOT exist!"
                      loop t
doAction (Just (Find w)) t = case (prefix w t) of
    Nothing -> do putStrLn "No words found with that prefix!"
                  loop t
    Just ws -> do putStrLn "Found words:"
                  (putStr . unlines) ws
                  loop t
doAction (Just Print) t = do putStrLn "List of words in dictionary:"
                             (putStr . unlines . getWords) t
                             loop t
doAction _ _ = return ()


loop :: Trie -> IO ()
loop t = do
        input <- getInput
        let action = convertAction input
        doAction action t


main = do
        args <- getArgs
        if null args then
            putStrLn "Please give a filename"
        else do
            let filename = args !! 0
            ws <- readFile filename
            loop $ insertList (lines ws)
