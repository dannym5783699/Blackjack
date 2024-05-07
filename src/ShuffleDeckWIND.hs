module ShuffleDeckWIND where

import System.Random
import CardDeckWIND (Deck(..))

-- Fisher-Yates shuffle algorithm implementation for WIND deck
shuffleDeckWIND :: Deck -> IO Deck
shuffleDeckWIND EmptyDeck = return EmptyDeck
shuffleDeckWIND (Deck cards) = do
    gen <- newStdGen
    return . Deck $ shuffle' cards (length cards) gen

shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' [] _ _ = []
shuffle' [x] _ _ = [x]
shuffle' xs n gen = let
    (j, newGen) = randomR (0, n-1) gen
    (lead, x:ys) = splitAt j xs
    in x : shuffle' (lead ++ ys) (n-1) newGen
