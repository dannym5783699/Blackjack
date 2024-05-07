module Main where

import           GUI
import           RegBlackJackGameMAC
import           RegBlackJackGameWIND
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--wind"] -> startGameLoopWIND
    ["--mac"]  -> startGameLoopMAC
    ["--gui"]  -> startGUI
    _          -> printErrorMessage

printErrorMessage :: IO ()
printErrorMessage = do
  putStrLn ""
  putStrLn "Available Commands: \n"
  putStrLn "  --wind  -  launches CLI app for windows users"
  putStrLn "  --mac   -  launches CLI app for mac users"
  putStrLn "  --gui   -  launches GUI app in external window\n"
  putStrLn "(Bash Ex: cabal run exes -- --gui)\n"
  putStrLn ""
