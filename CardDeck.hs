data Suit = Diamond | Heart | Spade | Club
  deriving(Eq, Enum)

instance Show Suit where
  show Diamond = "\x2666"
  show Heart   = "\x2665"
  show Spade   = "\x2663"
  show Club    = "\x2660"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving(Eq, Ord, Enum)

instance Show Rank where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"

data Card = Card Suit Rank
  deriving(Eq)

instance Show Card where
  show (Card a b) = "[" ++ show a ++ show b ++ "]"

type Deck = [Card]

createDeck :: Deck
createDeck = [Card suit rank | suit <- [Diamond .. Club], rank <- [Two .. Ace]]


