module ShuffleDeck where

import           CardDeckMAC
import           System.Random

-- Fisher-Yates shuffle algorithm implementation
shuffleDeck :: Deck -> IO Deck
shuffleDeck EmptyDeck = return EmptyDeck
shuffleDeck (Deck cards) = do
    gen <- newStdGen
    return . Deck $ shuffle' cards (length cards) gen

-- Helper function for the Fisher-Yates shuffle algorithm
shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' [] _ _ = []
shuffle' [x] _ _ = [x]
shuffle' xs n gen = let
    (j, newGen) = randomR (0, n-1) gen
    (lead, x:ys) = splitAt j xs
    in x : shuffle' (lead ++ ys) (n-1) newGen
