module ShuffleDeck where

import           CardDeckMAC 
import           System.Random -- Import the random number generation library

-- Fisher-Yates shuffle algorithm implementation
shuffleDeck :: Deck -> IO Deck
shuffleDeck EmptyDeck = return EmptyDeck -- If the deck is empty, return it unchanged
shuffleDeck (Deck cards) = do
    gen <- newStdGen  -- Generate a new random number generator
    return . Deck $ shuffle' cards (length cards) gen    -- Shuffle the deck using the shuffle' helper function.

-- Helper function for the Fisher-Yates shuffle algorithm
-- It takes a list of cards, the number of cards, and a random number generator as input.
shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' [] _ _ = []  -- If there are no cards, return an empty list.
shuffle' [x] _ _ = [x] -- If there is only one card, return it unchanged.
shuffle' xs n gen = let
    (j, newGen) = randomR (0, n-1) gen  -- Generate a random index 'j'.
    (lead, x:ys) = splitAt j xs  -- Split the list at index 'j', separating the selected card 'x'.
    in x : shuffle' (lead ++ ys) (n-1) newGen   -- Recursively shuffle the remaining cards, decreasing the count by one.
