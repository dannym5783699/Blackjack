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
- [How to Run GUI](#how-to-run-gui)
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
cabal run exes -- --gui
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

- The game tracks wins for the player
```txt 
Enter number of decks [1-6]
5
Enter your name:
Alex

Dealers Hand
[♦6] [*]
Players Hand
[♥Q] [♣9] 

Would you like a card? ('Y' or 'N')
n

Dealers Hand
[♦6] [♠9] [♦3] 
Players Hand
[♥Q] [♣9] 

You WON!
Current wins: 1
Would you like to play again? ('Y' or 'N')
y

Dealers Hand
[♣A] [*]
Players Hand
[♠8] [♠Q] 

Would you like a card? ('Y' or 'N')
n

Dealers Hand
[♣A] [♦2] [♥8] 
Players Hand
[♠8] [♠Q] 

The Dealer Wins
Current wins: 1
Would you like to play again? ('Y' or 'N')
y

Dealers Hand
[♥8] [*]
Players Hand
[♥10] [♦K] 

Would you like a card? ('Y' or 'N')
n

Dealers Hand
[♥8] [♣K] 
Players Hand
[♥10] [♦K] 

You WON!
Current wins: 2
Would you like to play again? ('Y' or 'N')
y

Dealers Hand
[♣Q] [*]
Players Hand
[♦5] [♠9] 

Would you like a card? ('Y' or 'N')
y

Your New Hand
[♦5] [♠9] [♦K] 


Dealers Hand
[♣Q] [♥10] 
Players Hand
[♦5] [♠9] [♦K] 

The Dealer Wins
Current wins: 2
Would you like to play again? ('Y' or 'N')
n
Total wins: 2
Thank you for Playing!

Basic Rules of Blackjack

-The primary goal of Blackjack s to beat the dealer's hand without your hand exceeding a total of 21 points. You want to get your card values added up to 21 or closer than the dealer.

Card Values:
-Number cards (2 through 10) are worth their face value.
-Face cards (Jack, Queen, King) are each worth 10 points.
-Aces can be worth either 1 or 11 points, depending on which is more favorable to the hand. You can choose!

How the Game works and is played

- Each player starts with two cards, as well as the dealer. One of the dealer’s cards is face up and one is face down.

- Players decide how to play their hands in turn, with options including 'Hit', 'Stand', 'Split', or 'Surrender'. Hit means get another card from the dealer. Stand means you saty with your cards not getting anymore. Split is allowede when you have two of the same card and you can choose to swith one of them.

-After all players have finished their turns, the dealer reveals their hole card. The dealer must hit if their total is 16 or less, and must stand on 17 or more.

How to determine a winner
-If you exceed 21 points, you and automatically lose.
-If you don’t exceed 21 and your total is higher than the dealer’s, you win.
-If the player and Delaer have the same value, the money betted is retunred to each player.

This game can be played with multiple people and multiple decks.

## Features
- Simulation of Blackjack games**: Play against an automated dealer.
- Card Counting Practice**: Includes features to assist in learning and practicing card counting.
- Customizable Settings**: Choose the number of decks and other game rules to simulate different casino settings.
- Statistical Analysis**: Review your play sessions with detailed stats on your win rate, the frequency of Blackjack, and more.
-Interactive CLI: Simple and intuitive command-line interface for easy navigation and game play.

## Configuration Options
Modify the game's settings to suit your practice needs, including:
- Number of Decks: Configure from 1 to 6 decks.





