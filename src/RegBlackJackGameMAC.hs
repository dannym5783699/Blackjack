module RegBlackJackGameMAC where

import           CardDeckMAC
import           Data.Char

import           PlayerData 

import ShuffleDeck 

readMatrix :: IO [[Char]]
readMatrix = do
    content <- readFile "docs\\decisionMatrix.txt"
    return (map (filter (/= ' ')) (lines content))


charToInt :: Char -> Int
charToInt x = case x of
                 '0' -> (0)
                 '1' -> 1
                 '2' -> 2
                 '3' -> 3
                 '4' -> 4
                 '5' -> 5
                 '6' -> 6
                 '7' -> 7
                 '8' -> 8
                 '9' -> 9
                 'A' -> 10
                 'B' -> 11
                 'C' -> 12
                 'D' -> 13
                 'F' -> 14
                 'G' -> 15
                 '+' -> 21
                 _ -> -1
                

dealerDecision :: Card -> [Char] -> IO ()
dealerDecision _ [] = return ()
dealerDecision x (y:ys) | snd (cardToInt x) == (charToInt y) = do
                           putStrLn [y]
                        | otherwise = dealerDecision   x ys 


startGameLoopMAC :: IO ()
startGameLoopMAC = do
  putStrLn "Enter number of decks [1-6]"
  char <- getChar
  _ <- getChar  -- consume the newline after input
  player <- createPlayer  -- Create a new player at the start of the game
  if isDigit char && char > '0' && char < '7' then do
    let num = digitToInt char
    playDeck <- shuffleDeck (createVarDeck num)  -- shuffle after creating the variable deck
    let discPile = EmptyDeck
    gameLoop player discPile playDeck
  else do
    putStrLn "Invalid input, defaulting to standard deck."
    originalDeck <- shuffleDeck createDeck
    let discPile = EmptyDeck
    gameLoop player discPile originalDeck



safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x


gameLoop :: Player -> Deck -> Deck -> IO ()
gameLoop player discPile playDeck
  | hasRemainingCards playDeck 4 = do
    let (playerHand1, dealerHand1, discPile1, playDeck1) = dealHand playDeck discPile
    printHands dealerHand1 playerHand1 False

    -- Check for blackjack conditions first
    if ((hasBlackJack playerHand1) && (hasBlackJack dealerHand1)) then do
      putStrLn "You both have BlackJack it is a Tie"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      resetGameLoop player discPile1 playDeck1

    else if hasBlackJack playerHand1 then do
      putStrLn "You have BLACKJACK!!"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      let updatedPlayer = addWin player -- Player wins if they have blackjack
      resetGameLoop updatedPlayer discPile1 playDeck1

    else if hasBlackJack dealerHand1 then do
      putStrLn "Dealer has Blackjack, you lose"
      printHands dealerHand1 playerHand1 True
      printResults dealerHand1 playerHand1
      resetGameLoop player discPile1 playDeck1

    else do
      -- Play out the turn if no immediate blackjacks
      (playerHand2, discPile2, playDeck2) <- handleSplits [playerHand1] discPile1 playDeck1 dealerHand1
      let (dealerHand2, discPile3, playDeck3) = takeDealerTurn dealerHand1 discPile2 playDeck2
      printMultiHands dealerHand2 playerHand2 True
      -- Determine result from normal play
      let result = mapResults dealerHand2 playerHand2
      --let result = determinResults dealerHand2 playerHand2
      let updatedPlayer = mapPlayerResult player result           
      putStrLn $ show result
      putStrLn $ "Current wins: " ++ show (wins updatedPlayer)
      printMultiResults dealerHand2 playerHand2
      resetGameLoop updatedPlayer discPile3 playDeck3

  | otherwise = do
    putStrLn "There are no cards left in the deck, Thank you for Playing!"
    putStrLn $ "Total wins: " ++ show (wins player)

playerResult :: Player -> Result -> Player
playerResult player result = case result of
                            PlayerWon -> addWin player -- Update win if player won normally
                            _ -> player

mapPlayerResult :: Player -> [Result] -> Player
mapPlayerResult player [] = player
mapPlayerResult player (x:xs) = mapPlayerResult (playerResult player x) xs 

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
                         let (deck1, disc1, play1) = dealCard (safeHead EmptyDeck (drop (length pHand - 2) pHand)) discPile playDeck
                         let (deck2, disc2, play2) = dealCard (safeHead EmptyDeck (drop (length pHand - 1) pHand)) disc1 play1
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
printMultiHands _ [] _ = return ()
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
                  (player, disc1, play1) <- takePlayerTurn x disc play
                  handleTurns xs (acc ++ [player]) disc1 play1



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
splitDeck decks | length decks == 1 = [Deck [( safeHead (Card Heart Error) (deckToCards(safeHead EmptyDeck decks)) )], Deck (drop 1 (deckToCards(safeHead EmptyDeck decks)))]
                | otherwise = (safeHead EmptyDeck decks) : splitDeck (drop 1 decks)


canSplit :: [Deck] -> Bool
canSplit decks | length decks < 1 = False
               | length decks == 1 = canSplitDeck (safeHead EmptyDeck decks)
               | otherwise = canSplit (drop 1 decks)

canSplitDeck :: Deck -> Bool
canSplitDeck EmptyDeck = False
canSplitDeck (Deck cards) | length cards == 2 = cardToInt (safeHead (Card Heart Error) cards) == cardToInt (safeHead (Card Heart Error) (drop 1 cards))
                          | length cards > 2 = cardToInt (safeHead (Card Heart Error) ends) == cardToInt (safeHead (Card Heart Error) (drop 1 ends))
                          | otherwise = False
                              where 
                                ends = drop (length cards - 2 ) cards
                          

-- Resets the game loop with the discard Pile and the play deck you want
resetGameLoop :: Player -> Deck -> Deck -> IO ()
resetGameLoop player discPile playDeck = do
  putStrLn "Would you like to play again? ('Y' or 'N')"
  char <- getChar
  if char == 'Y' || char == 'y' then do
    _ <- getChar  -- consume the newline
    gameLoop player discPile playDeck
  else do
    putStrLn $ "Total wins: " ++ show (wins player)
    putStrLn "Thank you for Playing!"

-- Prints the results in a readable format to the user
printResults :: Deck -> Deck -> IO ()
printResults dealerHand playerHand = do
  putStrLn ""
  putStrLn (show (determinResults dealerHand playerHand))
  putStrLn ""

