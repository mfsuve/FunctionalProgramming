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
