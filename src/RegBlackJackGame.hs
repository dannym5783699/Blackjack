module RegBlackJackGame where

import           CardDeck

startGameLoop :: IO ()
startGameLoop = do
  let fullDeck = createDeck
  let discPile = EmptyDeck
  gameLoop fullDeck discPile

gameLoop :: Deck -> Deck -> IO ()
gameLoop playDeck discPile
  | hasRemaingingCards playDeck 4 = do
      let (playerHand1, dealerHand1, discPile1, playDeck1) = dealHand playDeck discPile
      print ("Dealer Hand")
      putStrLn $ show (getFirstCard dealerHand1) ++ " [*]"
      print ("Player Hand")
      print playerHand1
      let (dealerHand2, discPile2, playDeck2) = takeDealerTurn dealerHand1 discPile1 playDeck1
      putStrLn $ "Dealers New Hand"
      putStrLn $ show dealerHand2
      print "Would you like another card? ('Y' or 'N')"
      char <- getChar
      if char == 'Y' || char == 'y'
      then do
        _ <- getChar
        gameLoop discPile2 playDeck2
      else print "Thank you for Playing!"
  | otherwise = print "There are no cards left in the deck, Thank you for Playing!"

-- Deals out the player and dealers hand alternating cards first to player then to the dealer
dealHand :: Deck -> Deck -> (Deck, Deck, Deck, Deck)
dealHand pDeck discPile = (pHand2, dHand2, rmDeck21, pDeck21)
 where
  (pHand1, rmDeck10, pDeck10) = dealCard EmptyDeck discPile pDeck
  (dHand1, rmDeck11, pDeck11) = dealCard EmptyDeck rmDeck10 pDeck10
  (pHand2, rmDeck20, pDeck20) = dealCard pHand1 rmDeck11 pDeck11
  (dHand2, rmDeck21, pDeck21) = dealCard dHand1 rmDeck20 pDeck20

-- Keeps Dealing the dealer cards until it has above a soft 17
takeDealerTurn :: Deck -> Deck -> Deck -> (Deck, Deck, Deck)
takeDealerTurn dealerHand discPile playDeck
  | not (doesDealerDraw dealerHand) = (dealerHand, discPile, playDeck)
  | otherwise = takeDealerTurn dealerHand1 discPile1 playDeck1
    where
      (dealerHand1, discPile1, playDeck1) = dealCard dealerHand discPile playDeck
