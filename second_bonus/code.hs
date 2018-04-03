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
allSameColor []   = True
allSameColor [c]  = True
allSameColor (c:cs@(c':cs')) = if cardColor c == cardColor c' then allSameColor cs else False
