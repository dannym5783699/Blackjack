module PlayerData
  ( Player(..)
  , createPlayer
  , addWin
  ) where


-- Definition of the Player data type
data Player = Player {
    playerName :: String,  -- Stores the player's name
    wins       :: Int      -- Stores the number of wins
} deriving (Show)

-- Function to create a new player with the provided name and 0 wins
createPlayer :: IO Player
createPlayer = do
    putStrLn "Enter your name:"
    name <- getLine
    return $ Player name 0  -- Initializes the player with 0 wins

-- Function to increment the win count of a player
addWin :: Player -> Player
addWin player = player { wins = wins player + 1 }

