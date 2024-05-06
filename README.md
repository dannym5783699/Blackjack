# Blackjack Card Counter
## Table of Contents
- [Prerequisites](#prerequisites)
  - [homebrew](#how-to-install-homebrew)
  - [ghc](#how-to-install-ghc)
  - [cabal](#how-to-install-cabal)
- [How to Build CLI](#how-to-build-cli)
- [How to Test a Module](#how-to-test-a-module)
- [How to Run CLI On Mac](#how-to-run-on-mac)
- [How to Run CLI on Windows](#how-to-run-on-windows)
- [How to Run GUI]()
- [How to play Regular Black Jack](#opperation-of-cli)

## Prerequisites 
  - [homebrew](#how-to-install-homebrew)
  - [ghc](#how-to-install-ghc)
  - [cabal](#how-to-install-cabal)

***Warning: For MAC Users ONLY***
### How to Install `homebrew`
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
### How to Install `ghc`
```bash
brew install ghc
```
### How to Install `cabal`
```bash
brew install ghc cabal-install
```

## How to Build CLI
### Build Game
```bash
cabal update
cabal build
```

## How to Test a Module with random
```bash
ghci -package random CardDeck.hs
```

## How to Run on Mac
### Run Game as CLI
```bash
cabal run exes -- --mac
```

## How to Run on Windows
### Run Game as CLI
```bash
cabal run exes -- --wind
```

## How to Run GUI
### Run Game in the GUI
```bash
cabal run exes -- --mac --gui
```

## Opperation of CLI
- Prints out the Dealers hand with one card represented as a flipped over card
- Prints out the Players Hand with both cards showing
- Hand is delt first card to player next card to dealer and so on until each person has two cards
- As long as the player can pick another card it will prompt the user to get the next card.
- Plays the Dealers Hand Taking New cards until the dealers hand has at least a soft 17
- Prints out the dealers full hand
- Asks the user if it would like to continue
- Determines the winner and looser at the end of each round
```txt

Dealers Hand
[♦3] [*]
Players Hand
[♦2] [♦4] 

Would you like a card? ('Y' or 'N')
n

Dealers Hand
[♦3] [♦5] [♦6] [♦7] 
Players Hand
[♦2] [♦4] 


The Dealer Wins

Would you like to play again? ('Y' or 'N')
y

Dealers Hand
[♦9] [*]
Players Hand
[♦8] [♦10] 

Would you like a card? ('Y' or 'N')
n

Dealers Hand
[♦9] [♦J] 
Players Hand
[♦8] [♦10] 


The Dealer Wins

Would you like to play again? ('Y' or 'N')
y

Dealers Hand
[♦K] [*]
Players Hand
[♦Q] [♦A] 

You have BLACKJACK!!

Dealers Hand
[♦K] [♥2] 
Players Hand
[♦Q] [♦A] 


You WON!

Would you like to play again? ('Y' or 'N')
n
"Thank you for Playing!"
```
- If the player or the dealer or both get BlackJack it will prevent the user from continueing and start a new round
```txt
Dealers Hand
[♦K] [*]
Players Hand
[♦Q] [♦A] 

You have BLACKJACK!!

Dealers Hand
[♦K] [♥2] 
Players Hand
[♦Q] [♦A] 

Would you like to play again? ('Y' or 'N')
n
"Thank you for Playing!"
```