module Main where

import           GUI
import           RegBlackJackGameMAC
import           RegBlackJackGameWIND
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--wind"]  -> startGameLoopWIND
    ["--mac"]   -> startGameLoopMAC
    [_,"--gui"] -> startGUI
    _           -> printErrorMessage

printErrorMessage :: IO ()
printErrorMessage = do
  putStrLn ""
  putStrLn "Available Commands:"
  putStrLn "\twind  -  launches CLI app for windows users"
  putStrLn "\tmac   -  launches CLI app for mac users"
