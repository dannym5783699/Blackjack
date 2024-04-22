module RegBlackJackGame where

import           CardDeck

startGameLoop :: IO ()
startGameLoop = do
  let fullDeck = createDeck
  let rmDeck = EmptyDeck
  gameLoop fullDeck rmDeck

gameLoop :: Deck -> Deck -> IO ()
gameLoop playDeck rmDeck
  | hasRemaingingCards playDeck 4 = do
      let (playerHand, dealerHand, newRMDeck, newPlayDeck) = dealHand playDeck rmDeck
      print ("Dealer Hand")
      putStrLn $ show (getFirstCard dealerHand) ++ " [*]"
      print ("Player Hand")
      print playerHand
      print "Would you like another card? ('Y' or 'N')"
      char <- getChar
      if char == 'Y' || char == 'y'
      then do
        char <- getChar
        gameLoop newPlayDeck newRMDeck
      else print "Thank you for Playing!"
  | otherwise = print "There are no cards left in the deck, Thank you for Playing!"


dealHand :: Deck -> Deck -> (Deck, Deck, Deck, Deck)
dealHand pDeck rmDeck = (pHand2, dHand2, rmDeck21, pDeck21)
 where
  (pHand1, rmDeck10, pDeck10) = dealCard EmptyDeck rmDeck pDeck
  (dHand1, rmDeck11, pDeck11) = dealCard EmptyDeck rmDeck10 pDeck10
  (pHand2, rmDeck20, pDeck20) = dealCard pHand1 rmDeck11 pDeck11
  (dHand2, rmDeck21, pDeck21) = dealCard dHand1 rmDeck20 pDeck20
