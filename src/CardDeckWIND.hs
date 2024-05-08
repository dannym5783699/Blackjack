module CardDeckWIND where
import           System.Random

printRNDNum :: IO ()
printRNDNum = do
  num <- randomRIO (1, 100) :: IO Int
  putStrLn $ "Random number between 1 and 100: " ++ show num

data Suit = Diamond | Heart | Spade | Club
  deriving(Eq, Enum)

instance Show Suit where
  show Diamond = "\x0004"
  show Heart   = "\x0003"
  show Spade   = "\x0006"
  show Club    = "\x0005"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Error
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
  show Error = "E"

data Card = Card Suit Rank
  deriving(Eq)

instance Show Card where
  show (Card a b) = "[" ++ show a ++ show b ++ "]"

data Deck = EmptyDeck | Deck [Card]

instance Show Deck where
  show EmptyDeck           = "[]"
  show (Deck [])           = ""
  show (Deck (card:cards)) = show card ++ " " ++ show (Deck cards)

createDeck :: Deck
createDeck = Deck [Card suit rank | suit <- [Diamond .. Club], rank <- [Two .. Ace]]

createVarDeck :: Int -> Deck
createVarDeck 1 = createDeck
createVarDeck n = addDecks createDeck (createVarDeck (n-1))


addDecks :: Deck -> Deck -> Deck
addDecks EmptyDeck n       = n
addDecks n EmptyDeck       = n
addDecks (Deck n) (Deck c) = Deck (n ++ c)

getFirstCard :: Deck -> Card
getFirstCard (Deck (card:_)) = card
getFirstCard _ = undefined "Error, no cards"


hasRemainingCards :: Deck -> Int -> Bool
hasRemainingCards EmptyDeck _ = False
hasRemainingCards (Deck cards) n = length cards >= n


takeXCards :: Deck -> Int -> Deck
takeXCards EmptyDeck _    = EmptyDeck
takeXCards (Deck []) _    = EmptyDeck
takeXCards (Deck cards) n = Deck (take n cards)


addToRemovedDeck :: Deck -> Deck -> Deck
addToRemovedDeck (Deck nrmCards) (Deck rmCards) = Deck (nrmCards ++ rmCards)
addToRemovedDeck (Deck nrmCards) EmptyDeck = Deck (nrmCards)
addToRemovedDeck EmptyDeck EmptyDeck = EmptyDeck
addToRemovedDeck EmptyDeck (Deck rmCards) = Deck(rmCards)


removeCard :: Deck -> Int -> Deck
removeCard (Deck cards) x = Deck (drop x cards)
removeCard EmptyDeck _ = EmptyDeck


dealCard :: Deck -> Deck -> Deck -> (Deck, Deck, Deck)
dealCard EmptyDeck EmptyDeck (Deck playCards) = (Deck (take 1 playCards), Deck (take 1 playCards), Deck (drop 1 playCards))
dealCard EmptyDeck (Deck rCards) (Deck playCards) = (Deck (take 1 playCards), Deck (rCards ++ take 1 playCards), Deck (drop 1 playCards))
dealCard (Deck hand) (Deck rCards) (Deck playCards) = (Deck (hand ++ take 1 playCards), Deck (rCards ++ take 1 playCards), Deck (drop 1 playCards))
dealCard xs bs EmptyDeck = (xs, bs, EmptyDeck)
dealCard _ _ _ = undefined "Deal card error."


-- Returns a tupal of the cards, first value is low value, second value is high value
cardToInt :: Card -> (Int, Int)
cardToInt (Card _ r1) = case r1 of
  Two   -> (2,2)
  Three -> (3,3)
  Four  -> (4,4)
  Five  -> (5,5)
  Six   -> (6,6)
  Seven -> (7,7)
  Eight -> (8,8)
  Nine  -> (9,9)
  Ten   -> (10,10)
  Jack  -> (10,10)
  Queen -> (10,10)
  King  -> (10,10)
  Ace   -> (1,11)
  Error -> (-1, -1)

-- Gets the value of the hand, first value is the low value, second value is the high value
getHandValue :: Deck -> (Int,Int)
getHandValue EmptyDeck = (0,0)
getHandValue (Deck cards) = foldr (\card -> addCardValues (cardToInt card)) (0,0) cards

-- Adds the Value of two Cards, but in int form
addCardValues :: (Int,Int) -> (Int,Int) -> (Int,Int)
addCardValues (a,b) (c,d) = (a + c, b + d)

-- Tells the dealer to draw on anything above a soft 17
doesDealerDraw :: Deck -> Bool
doesDealerDraw hand = go (getHandValue hand)
  where
    go (_,h) = h < 17

-- Returns True if the players hand is less then a hard 21
canPlayerPlay :: Deck -> Bool
canPlayerPlay hand = go (getHandValue hand)
  where
    go (l,_) = l < 21

hasPlayable :: [Deck] -> Bool
hasPlayable [] = False
hasPlayable xs = or (map canPlayerPlay xs)



-- Determines if the hand equals 21 or BlackJack
hasBlackJack :: Deck -> Bool
hasBlackJack hand = go (getHandValue hand)
 where
  go (_,h) = h == 21

-- Used to keep track of the results of each round
data Result = DealerWon | PlayerWon | BothOver | Tie Int

-- Used to Print out a message of the results to the user
instance Show Result where
  show DealerWon = "The Dealer Wins"
  show PlayerWon = "You WON!"
  show BothOver  = "Both you and the dealer went over 21"
  show (Tie n)   = "It was a draw with value: " ++ show n

-- Determines the results of each round
determinResults :: Deck -> Deck -> Result
determinResults dealerHand playerHand = go (getHandValue dealerHand) (getHandValue playerHand)
  where
    go (ld, hd) (lp, hp)
      | ld > 21 && lp > 21 = BothOver
      | hd <= 21 && hd == hp = Tie hd
      | hd > 21 && ld == lp = Tie ld
      | (ld > 21) || (hp <=21 && hd > 21 && ld < hp) || (hp > 21 && hd > 21 && ld < lp) || (hd <= 21 && hp <= 21 && hd < hp)= PlayerWon
      | otherwise = DealerWon

mapResults :: Deck -> [Deck] -> [Result]
mapResults _ [] = []
mapResults dealer (x:xs) = (determinResults dealer x) : (mapResults dealer xs)


deckToCards :: Deck -> [Card]
deckToCards EmptyDeck = []
deckToCards (Deck as) = as

