module CardDeck where

import           System.Random

printRNDNum :: IO ()
printRNDNum = do
  num <- randomRIO (1, 100) :: IO Int
  putStrLn $ "Random number between 1 and 100: " ++ show num

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
  show Ace   = "A"

data Card = Card Suit Rank
  deriving(Eq)

instance Show Card where
  show (Card a b) = "[" ++ show a ++ show b ++ "]"

data Deck = EmptyDeck | Deck [Card]

instance Show Deck where
  show EmptyDeck = "[]"
  show (Deck (card:cards))
    | cards == [] = show card
    | otherwise = show card ++ " " ++ show (Deck cards)

createDeck :: Deck
createDeck = Deck [Card suit rank | suit <- [Diamond .. Club], rank <- [Two .. Ace]]

getFirstCard :: Deck -> Card
getFirstCard (Deck (card:cards)) = card

hasRemainingCards :: Deck -> Int -> Bool
hasRemainingCards (Deck []) _ = False
hasRemaingingCards (Deck cards) n
  | length cards < n = False
  | otherwise = True

takeXCards :: Deck -> Int -> Deck
takeXCards (Deck cards) n = Deck (take n cards)

addToRemovedDeck :: Deck -> Deck -> Deck
addToRemovedDeck (Deck nrmCards) (Deck rmCards) = Deck (nrmCards ++ rmCards)

removeCard :: Deck -> Int -> Deck
removeCard (Deck cards) x = Deck (drop x cards)

dealCard :: Deck -> Deck -> Deck -> (Deck, Deck, Deck)
dealCard EmptyDeck EmptyDeck (Deck playCards) = (Deck (take 1 playCards), Deck (take 1 playCards), Deck (drop 1 playCards))
dealCard EmptyDeck (Deck rCards) (Deck playCards) = (Deck (take 1 playCards), Deck (rCards ++ take 1 playCards), Deck (drop 1 playCards))
dealCard (Deck hand) (Deck rCards) (Deck playCards) = (Deck (hand ++ take 1 playCards), Deck (rCards ++ take 1 playCards), Deck (drop 1 playCards))


