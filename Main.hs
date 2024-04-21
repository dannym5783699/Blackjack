import           CardDeck
main :: IO ()
main = do
  startGameLoop

startGameLoop :: IO ()
startGameLoop = do
  let fullDeck = createDeck
  let rmDeck = Deck ([])
  gameLoop fullDeck rmDeck

gameLoop :: Deck -> Deck -> IO ()
gameLoop playDeck rmDeck
  | hasRemaingingCards playDeck 2 = do
      let hand = takeXCards playDeck 2
      let newRMDeck = addToRemovedDeck hand rmDeck
      let newPlayDeck =  removeCard playDeck 2
      print (hand)
      print "Would you like another card? ('Y' or 'N')"
      char <- getChar
      if char == 'Y' || char == 'y'
      then do
        char <- getChar
        gameLoop newPlayDeck newRMDeck
      else print "Thank you for Playing!"
  | otherwise = print "There are no cards left in the deck, Thank you for Playing!"
