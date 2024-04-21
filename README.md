# Blackjack Card Counter
## Table of Contents
- [Prerequisites](#prerequisites)
  - [homebrew](#how-to-install-homebrew)
  - [ghc](#how-to-install-ghc)
  - [cabal](#how-to-install-cabal)
  - [System.Random](#how-to-install-systemrandom)
- [How to Build and Run CLI](#how-to-build-and-run-cli)
- [How to Test a Module](#how-to-test-a-module)
- [How to play Regular Black Jack](#opperation-of-cli)

## Prerequisites 
  - [homebrew](#how-to-install-homebrew)
  - [ghc](#how-to-install-ghc)
  - [cabal](#how-to-install-cabal)
  - [System.Random](#how-to-install-systemrandom)

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
### How to Install `System.Random`
```bash
cabal update
cabal install --lib random
```

## How to Build and Run CLI
### Build Game
```bash
ghc -package random Main.hs
```
### Run Game
```bash
./Main
```

## How to Test a Module
```bash
ghci -package random CardDeck.hs
```

## Opperation of CLI
- CLI will print out the top two cards in a deck
- CLI will the prompt the user if they want to continue
- Will ten print the next two cards in the deck