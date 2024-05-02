# Blackjack Card Counter
## Table of Contents
- [Prerequisites](#prerequisites)
  - [homebrew](#how-to-install-homebrew)
  - [ghc](#how-to-install-ghc)
  - [cabal](#how-to-install-cabal)
- [How to Build and Run CLI](#how-to-build-and-run-cli)
- [How to Test a Module](#how-to-test-a-module)
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

## How to Build and Run CLI
### Build Game
```bash
cabal build
```
### Run Game
```bash
cabal run
```

## How to Test a Module with random
```bash
ghci -package random CardDeck.hs
```

## Opperation of CLI
- Prints out the Dealers hand with one card represented as a flipped over card
- Prints out the Players Hand with both cards showing
- Hand is delt first card to player next card to dealer and so on until each person has two cards
- Plays the Dealers Hand Taking New cards until the dealers hand has at least a soft 17
- Prints out the dealers full hand
- Asks the user if it would like to continue
```txt
"Dealer Hand"
[♦3] [*]
"Player Hand"
[♦2] [♦4]
Dealers New Hand
[♦3] [♦5] [♦6] [♦7] 
"Would you like another card? ('Y' or 'N')"
```