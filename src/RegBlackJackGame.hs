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
    printHands dealerHand1 playerHand1 False
    (playerHand2, discPile2, playDeck2) <- takePlayerTurn playerHand1 discPile1 playDeck1
    let (dealerHand2, discPile3, playDeck3) = takeDealerTurn dealerHand1 discPile2 playDeck2
    printHands dealerHand2 playerHand2 True
    print "Would you like to play again? ('Y' or 'N')"
    char <- getChar
    if char == 'Y' || char == 'y'
    then do
      _ <- getChar
      gameLoop playDeck3 discPile3
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

-- Opperates the functionality of the Players Turn
takePlayerTurn :: Deck -> Deck -> Deck -> IO (Deck, Deck, Deck)
takePlayerTurn playerHand discPile playDeck
  | canPlayerPlay playerHand = do
    putStrLn "Would you like a card? ('Y' or 'N')"
    char <- getChar
    if char == 'Y' || char == 'y'
    then do
      _ <- getChar
      let (playerHand1, discPile1, playDeck1) = dealCard playerHand discPile playDeck
      _ <- putStrLn ""
      _ <- putStrLn "Your New Hand"
      _ <- putStrLn (show playerHand1)
      _ <- putStrLn ""
      takePlayerTurn playerHand1 discPile1 playDeck1
    else do
      _ <- getChar
      return (playerHand, discPile, playDeck)
  | otherwise = do
    return (playerHand, discPile, playDeck)

-- Prints the hands Dealers first
printHands :: Deck -> Deck -> Bool-> IO ()
printHands dealerHand playerHand showDealersFullHand = do
  putStrLn ""
  putStrLn "Dealers Hand"
  if showDealersFullHand then
    putStrLn (show dealerHand)
  else
    putStrLn (show (getFirstCard dealerHand) ++ " [*]")
  putStrLn "Players Hand"
  putStrLn (show playerHand)
  putStrLn ""
