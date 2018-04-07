module Main where
import Data.Char

data Color = Red | Black
  deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace
  deriving (Show, Eq)
data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Show, Eq)
data Move = Draw | Discard Card
  deriving (Show, Eq)
data State = State {held :: [Card], list :: [Card]}
  deriving (Show, Eq)

cardColor :: Card -> Color
cardColor card
  | suit card == Spades = Black
  | suit card == Clubs  = Black
  | otherwise           = Red

cardValue :: Card -> Int
cardValue card = case rank card of
  Num n -> n
  Ace   -> 11
  _     -> 10
  
removeCard :: [Card] -> Card -> [Card]
removeCard [] c = error "Card is not in the list"
removeCard (c':cs) c
  | c' == c     = cs
  | otherwise   = c' : removeCard cs c
  
allSameColor :: [Card] -> Bool
allSameColor []              = True
allSameColor [c]             = True
allSameColor (c:cs@(c':cs')) = if cardColor c == cardColor c' then allSameColor cs else False

sumCards :: [Card] -> Int
sumCards cs = sumCardstail cs 0
  where
    sumCardstail :: [Card] -> Int -> Int
    sumCardstail []       acc = acc
    sumCardstail (c':cs') acc = sumCardstail cs' (acc + cardValue c')  -- Parenthesis are for avoiding this situation: (sumCardstail cs' acc) + cardValue c'

score :: [Card] -> Int -> Int
score cs g
  | allSameColor cs = p `div` 2
  | otherwise       = p
    where
      p :: Int
      p = if s > g then 3 * (s - g) else g - s
        where
          s :: Int
          s = sumCards cs

runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms g = runGame' ms (State [] cs)
  where
    runGame' :: [Move] -> State -> Int
    runGame' ms' state
      | isOver    = score (held state) g
      | otherwise = nextMove
        where
          isOver :: Bool
          isOver = case (ms', state) of
            ([],     _             ) -> True                                   -- No moves left
            (Draw:_, State{list=[]}) -> True                                   -- Drawing from empty card list
            (_,      State{held=h} ) -> if sumCards h > g then True else False -- Sum of the held-cards exceeds the goal

          nextMove :: Int
          nextMove = case (ms', state) of
            ((Discard c):ms'', State{held=h, list=l}    ) -> runGame' ms'' (State (removeCard h c) l) -- Discarding a card
            ( Draw:ms'',       State{held=h, list=c:cs'}) -> runGame' ms'' (State (c:h) cs')          -- Drawing a card

convertSuit :: Char -> Suit
convertSuit c
  | c `elem` "cC" = Clubs
  | c `elem` "dD" = Diamonds
  | c `elem` "hH" = Hearts
  | c `elem` "sS" = Spades
  | otherwise     = error "Unknown Suit"

convertRank :: Char -> Rank
convertRank c
  | c `elem` "jJ"           = Jack
  | c `elem` "qQ"           = Queen
  | c `elem` "kK"           = King
  | c `elem` "1"            = Ace
  | c `elem` "tT"           = Num 10
  | isDigit c && digit /= 0 = Num digit
  | otherwise               = error "Unknown Rank"
    where
      digit = digitToInt c

convertCard :: Char -> Char -> Card
convertCard s r = Card{suit = convertSuit s, rank = convertRank r}

readCards :: IO [Card]
readCards = readCards' []
  where
    readCards' :: [Card] -> IO [Card]
    readCards' cs = do input <- getLine
                       if input == "."
                          then return cs
                          else readCards' ((readCard input):cs)
                               where
                                 readCard :: String -> Card
                                 readCard [c1, c2] = convertCard c1 c2
                                 readCard _        = error "Wrong Input"
                                 
convertMove :: Char -> Char -> Char -> Move
convertMove m s r
  | m `elem` "dD" = Draw
  | m `elem` "rR" = Discard (convertCard s r)
  | otherwise     = error "Unknown Move"
  
readMoves :: IO [Move]
readMoves = readMoves' []
  where
    readMoves' :: [Move] -> IO [Move]
    readMoves' ms = do input <- getLine
                       if input == "."
                          then return ms
                          else readMoves' ((readMove input):ms)
                               where
                                 readMove :: String -> Move
                                 readMove [c1]         = convertMove c1 'x' 'x'
                                 readMove [c1, c2, c3] = convertMove c1 c2 c3
                                 readMove _            = error "Wrong Input"

main = do putStrLn "Enter cards:"
          cards <- readCards
          putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          putStrLn (show moves)

          putStrLn "Enter goal:"
          line <- getLine

          let goal = read line :: Int

          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
