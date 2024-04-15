import           CardDeck
main :: IO ()
main = do
  gameLoop

gameLoop :: IO ()
gameLoop = do
  let deck = createDeck
  print (getFirstCard deck)
  print "Would you like another card?"
  char <- getChar
  if char == 'y'
    then do
      char <- getChar
      gameLoop
    else print "Thank you for playing!"
