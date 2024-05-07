module RegBlackJackGameMAC where

import           CardDeckMAC
import           Data.Char

import ShuffleDeck 

startGameLoopMAC :: IO ()
startGameLoopMAC = do
  putStrLn "Enter number of decks [1-6]"
  char <- getChar
  if (isDigit char && char > '0' && char < '7') then do
    _ <- getChar  -- consume the newline after input
    let num = digitToInt char
    playDeck <- shuffleDeck (createVarDeck num)  -- shuffle after creating the variable deck
    let discPile = EmptyDeck
    gameLoop discPile playDeck
  else do
    putStrLn "Invalid input, defaulting to standard deck."
    originalDeck <- shuffleDeck createDeck  -- correct usage here
    let discPile = EmptyDeck
    gameLoop discPile originalDeck






-- The main loop for the game that starts that plays each rounc
gameLoop :: Deck -> Deck -> IO ()
gameLoop discPile playDeck
  | hasRemaingingCards playDeck 4 = do
    let (playerHand1, dealerHand1, discPile1, playDeck1) = dealHand playDeck discPile
    printHands dealerHand1 playerHand1 False

    if ((hasBlackJack playerHand1) && (hasBlackJack dealerHand1)) then do
      putStrLn "You both have BlackJack it is a Tie"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      resetGameLoop discPile1 playDeck1

    else if hasBlackJack playerHand1 then do
      putStrLn "You have BLACKJACK!!"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      resetGameLoop discPile1 playDeck1

    else if hasBlackJack dealerHand1 then do
      putStrLn "Dealer has Blackjack, you loose"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      resetGameLoop discPile1 playDeck1

    else do
      (playerHand2, discPile2, playDeck2) <- handleSplits [playerHand1] discPile1 playDeck1 dealerHand1
      let (dealerHand2, discPile3, playDeck3) = takeDealerTurn dealerHand1 discPile2 playDeck2
      printMultiHands dealerHand2 playerHand2 True
      printMultiResults dealerHand2 playerHand2
      resetGameLoop discPile3 playDeck3

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



handleSplits :: [Deck] -> Deck -> Deck -> Deck -> IO ([Deck], Deck, Deck)
handleSplits playerHands discPile playDeck dealer
          | hasPlayable playerHands = do
                 if canSplit playerHands then do
                     putStrLn "Would you like to split? ('Y' or 'N')"
                     char <- getChar
                     _ <- getChar
                     if char == 'Y' || char == 'y' then do
                         let pHand = splitDeck playerHands
                         let (deck1, disc1, play1) = dealCard (head (drop (length pHand - 2) pHand)) discPile playDeck
                         let (deck2, disc2, play2) = dealCard (head (drop (length pHand - 1) pHand)) disc1 play1
                         let finalHand = take (length pHand - 2) pHand ++ [deck1] ++ [deck2]
                         printMultiHands dealer finalHand False
                         handleSplits finalHand disc2 play2 dealer
                      else do
                        handleTurns playerHands [] discPile playDeck
                  else do
                    handleTurns playerHands [] discPile playDeck         
          | otherwise = do
              return (playerHands, discPile, playDeck)          


printMultiHands :: Deck -> [Deck] -> Bool -> IO ()
printMultiHands dealer [] _ = return ()
printMultiHands dealer (x:xs) b = do 
                              printHands dealer x b
                              printMultiHands dealer xs b


printMultiResults :: Deck -> [Deck] -> IO ()
printMultiResults _ [] = return ()
printMultiResults dealer (x:xs) = do
                         printResults dealer x
                         printMultiResults dealer xs


handleTurns :: [Deck] -> [Deck] -> Deck -> Deck -> IO ([Deck], Deck, Deck)
handleTurns [] acc disc play = return (acc, disc, play)
handleTurns (x:xs) acc disc play = do
                  (player, disc, play) <- takePlayerTurn x disc play
                  handleTurns xs (acc ++ [player]) disc play 



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


splitDeck :: [Deck] -> [Deck]
splitDeck decks | length decks == 1 = [Deck (deckToCards(head decks)), Deck (deckToCards(head (tail decks)))]
                | otherwise = (head decks) : splitDeck (tail decks)


canSplit :: [Deck] -> Bool
canSplit decks | length decks < 1 = False
               | length decks == 1 = canSplitDeck (head decks)
               | otherwise = canSplit (tail decks)

canSplitDeck :: Deck -> Bool
canSplitDeck EmptyDeck = False
canSplitDeck (Deck cards) | length cards == 2 = cardToInt (head cards) == cardToInt (head (tail cards))
                          | length cards > 2 = cardToInt (head ends) == cardToInt (head (tail ends))
                          | otherwise = False
                              where 
                                ends = drop (length cards - 2 ) cards
                          

-- Resets the game loop with the discard Pile and the play deck you want
resetGameLoop :: Deck -> Deck -> IO ()
resetGameLoop discPile playDeck = do
  putStrLn "Would you like to play again? ('Y' or 'N')"
  char <- getChar
  if char == 'Y' || char == 'y'
  then do
    _ <- getChar
    gameLoop discPile playDeck
  else print "Thank you for Playing!"

-- Prints the results in a readable format to the user
printResults :: Deck -> Deck -> IO ()
printResults dealerHand playerHand = do
  putStrLn ""
  putStrLn (show (determinResults dealerHand playerHand))
  putStrLn ""
