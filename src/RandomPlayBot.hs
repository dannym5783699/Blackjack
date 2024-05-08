module RandomPlayBot where

import System.Random
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import CardDeckMAC
import PlayerData
import RegBlackJackGameMAC  -- Assuming it includes necessary gameplay functions

-- Main function to start the bot
startBotGame :: IO ()
startBotGame = do
    putStrLn "Bot starting a new game"

-- Bot game loop, simulating gameplay
botGameLoop :: Player -> Deck -> Deck -> IO ()
botGameLoop player discardPile deck = do
    when (hasRemainingCards deck 4) $ do
        let (playerHand, dealerHand, newDiscardPile, newDeck) = dealHand deck discardPile
        putStrLn "Deck after dealing cards:"
        print newDeck
        -- Simulate the player turn
        (finalPlayerHand, finalDiscardPile, finalDeck) <- botDecisionLoop playerHand newDiscardPile newDeck
        putStrLn "Round completed. Results beckon:"
        printResults dealerHand finalPlayerHand
        -- Optionally add delays or additional logic
        botGameLoop player finalDiscardPile finalDeck

-- Bot makes decisions in a loop, deciding randomly to hit or stand
botDecisionLoop :: Deck -> Deck -> Deck -> IO (Deck, Deck, Deck)
botDecisionLoop playerHand discardPile deck = do
    decision <- randomRIO (0, 1 :: Int)  -- 0 for stand, 1 for hit
    if decision == 1 && canPlayerPlay playerHand then do
        putStrLn "Bot decides to hit."
        let (newPlayerHand, newDiscardPile, newDeck) = dealCard playerHand discardPile deck
        putStrLn "Deck after hitting:"
        print newDeck
        threadDelay 1000000  -- Delay for better simulation visibility
        botDecisionLoop newPlayerHand newDiscardPile newDeck
    else do
        putStrLn "Bot decides to stand, transcending the simple play."
        return (playerHand, discardPile, deck)
