module GUI where

import           CardDeckMAC
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           RegBlackJackGameMAC
import           ShuffleDeck


data World = World {playerHand         :: [Deck]
                    , dealerHand       :: Deck
                    , discPile         :: Deck
                    , playDeck         :: Deck
                    , playerTurn       :: Bool
                    , resultMessage    :: String
                    , blackJackResults :: (Bool,Bool)
                    , deckSelection    :: Bool
                    , possibleDecks    :: [Deck]
                    , splitOption      :: Bool
                    }
  deriving (Show)

setWorld :: World -> Picture
setWorld world
  | deckSelection world = pictures [titlePicture, numDecksWindow]

  | splitOption world = pictures ([titlePicture, dealerLabel, playerLabel, hitMeButton, stayButton, splitButton]
                                ++ (cardPictures True 120 (dealerHand world))
                                ++ (cardPictures False (-60) ((playerHand world) !! 0)))

  | playerTurn world = pictures ([titlePicture, dealerLabel, playerLabel, hitMeButton, stayButton]
                                ++ (cardPictures True 120 (dealerHand world))
                                ++ (cardPictures False (-60) ((playerHand world) !! 0)))

  | otherwise = pictures ([titlePicture, dealerLabel, playerLabel, nextCardButton, (resultsPanel (resultMessage world) (blackJackResults world))]
                          ++ (cardPictures False 120 (dealerHand world))
                          ++ (cardPictures False (-60) ((playerHand world) !! 0)))

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) world
  | (deckSelection world) && y >= -40 && y <= 0 =
    if (x >= (-170) && x <= (-130)) then getInitialHand 1 world
    else if (x >= (-110) && x <= (-70)) then getInitialHand 2 world
    else if (x >= (-50) && x <= (-10)) then getInitialHand 3 world
    else if (x >= 10 && x <= 50) then getInitialHand 4 world
    else if (x >= 70 && x <= 110) then getInitialHand 5 world
    else if (x >= 130 && x <= 170) then getInitialHand 6 world
    else world
  | playerTurn world && x >= 100 && x <= 300 =
    if (y >= 50 && y <= 100) then hitPlayer world
    else if (y >= (-50) && y <= (0)) then showResults world
    else if (y >= (-150) && y <= (-100)) then splitHand world
    else world
  | otherwise =
    if (x >= 100 && x <= 300 && y >= 0 && y <= 50) then do
      getNextHand world
    else world

handleEvent _ world = world

splitHand :: World -> World
splitHand world = world {playerHand = playerHNew
                        , discPile = discPNew
                        , playDeck = playDNew
                        , playerTurn = shouldTakeTurn
                        , blackJackResults = (playerBlackJack,dealerBlackJack)
                        , resultMessage = newResultsMessage
                        , splitOption = guiCanSplitDeck (playerHNew !! 0)}
    where
      splitDecks = guiSplitDeck (playerHand world)
      (playerH, discPNew, playDNew) = getSplitDecks (take 2 splitDecks) [] (discPile world) (playDeck world)
      playerHNew = playerH ++ (drop 2 splitDecks)
      (playerBlackJack, dealerBlackJack) = (hasBlackJack (playerHNew !! 0), hasBlackJack (dealerHand world))
      newResultsMessage = show (determinResults (dealerHand world) (playerHNew !! 0))
      shouldTakeTurn = not (playerBlackJack || dealerBlackJack)

getSplitDecks :: [Deck] -> [Deck] -> Deck -> Deck -> ([Deck], Deck, Deck)
getSplitDecks [] accDeck dPile pDeck = (accDeck,dPile,pDeck)
getSplitDecks (pHand:pHands) [] dPile pDeck = getSplitDecks pHands [pHandNew] discPileNew playDeckNew
  where
    (pHandNew, discPileNew, playDeckNew) = dealCard pHand dPile pDeck
getSplitDecks (pHand:pHands) accDeck dPile pDeck = getSplitDecks pHands (accDeck ++ [pHandNew]) discPileNew playDeckNew
  where
    (pHandNew, discPileNew, playDeckNew) = dealCard pHand dPile pDeck

guiSplitDeck :: [Deck] -> [Deck]
guiSplitDeck []                          = []
guiSplitDeck (EmptyDeck:_)               = []
guiSplitDeck ((Deck []):_)               = []
guiSplitDeck ((Deck (card:cards)):decks) = Deck [card] : [Deck (cards)] ++ decks

getInitialHand :: Int -> World -> World
getInitialHand n world = world {playerHand = [playerHandNew]
              , dealerHand = dealerHandNew
              , discPile = discPileNew
              , playDeck = playDeckNew
              , playerTurn = shouldTakeTurn
              , deckSelection = False
              , blackJackResults = (playerBlackJack, dealerBlackJack)
              , resultMessage = resultMessageNew
              , splitOption = ((guiCanSplitDeck playerHandNew) && shouldTakeTurn)}
  where
    (playerHandNew, dealerHandNew, discPileNew, playDeckNew) = dealHand ((possibleDecks world) !! (n - 1)) (discPile world)
    resultMessageNew = show (determinResults dealerHandNew playerHandNew)
    (playerBlackJack, dealerBlackJack) = (hasBlackJack playerHandNew, hasBlackJack dealerHandNew)
    shouldTakeTurn = (not (playerBlackJack || dealerBlackJack))

guiCanSplitDeck :: Deck -> Bool
guiCanSplitDeck EmptyDeck    = False
guiCanSplitDeck (Deck [])    = False
guiCanSplitDeck (Deck cards) = cardEqual (cards !! 0) (cards !! 1)

cardEqual :: Card -> Card -> Bool
cardEqual (Card _ r1) (Card _ r2)
  | r1 == r2 = True
  | r1 == Ten && (r2 == Jack || r2 == Queen || r2 == King) = True
  | r1 == Jack && (r2 == Ten || r2 == Queen || r2 == King) = True
  | r1 == Queen && (r2 == Ten || r2 == Jack || r2 == King) = True
  | r1 == King && (r2 == Ten || r2 == Jack || r2 == Queen) = True
  | otherwise = False

getShuffledDecks :: Int -> IO Deck
getShuffledDecks n = do
  shuffledDeck <- shuffleDeck (createVarDeck n)
  return shuffledDeck

hitPlayer :: World -> World
hitPlayer world =
  if (canPlayerPlay ((playerHand world) !! 0)) then
    if (canPlayerPlay (playerHNew)) then world {playerHand = [playerHNew]
                                                    , discPile = discPNew
                                                    , playDeck = playDNew}
    else showResults (world {playerHand = [playerHNew]
                                                    , discPile = discPNew
                                                    , playDeck = playDNew})
  else showResults world
    where
      (playerHNew, discPNew, playDNew) = dealCard ((playerHand world) !! 0) (discPile world) (playDeck world)

showResults :: World -> World
showResults world = world {playerTurn = False
                          , discPile  = discPileNew
                          , dealerHand = dealerHandNew
                          , playDeck = playDeckNew
                          , splitOption = False
                          , resultMessage = resultMessageNew}
  where
    (dealerHandNew, discPileNew, playDeckNew) = takeDealerTurn (dealerHand world) (discPile world) (playDeck world)
    resultMessageNew = show (determinResults (dealerHandNew) ((playerHand world) !! 0))


getNextHand :: World -> World
getNextHand world
  | length (playerHand world) > 1 = world {playerHand = nextPlayerHand
                                          , playerTurn = nextShouldTakeTurn
                                          , blackJackResults = (nextPlayerBlackJack,nextDealerBlackJack)
                                          , resultMessage = nextResultMessageNew
                                          , splitOption = ((guiCanSplitDeck (nextPlayerHand !! 0)) && shouldTakeTurn)}
  |otherwise = world {playerHand = [playerHandNew]
              , dealerHand = dealerHandNew
              , discPile = discPileNew
              , playDeck = playDeckNew
              , playerTurn = shouldTakeTurn
              , blackJackResults = (playerBlackJack, dealerBlackJack)
              , resultMessage = resultMessageNew
              , splitOption = ((guiCanSplitDeck playerHandNew) && shouldTakeTurn)}
  where
    (playerHandNew, dealerHandNew, discPileNew, playDeckNew) = dealHand (playDeck world) (discPile world)
    resultMessageNew = show (determinResults dealerHandNew playerHandNew)
    (playerBlackJack, dealerBlackJack) = (hasBlackJack playerHandNew, hasBlackJack dealerHandNew)
    shouldTakeTurn = (not (playerBlackJack || dealerBlackJack))
    nextPlayerHand = drop 1 (playerHand world)
    (nextPlayerBlackJack, nextDealerBlackJack) = (hasBlackJack (nextPlayerHand !! 0), hasBlackJack (dealerHand world))
    nextShouldTakeTurn = (not (nextPlayerBlackJack || nextDealerBlackJack))
    nextResultMessageNew = show (determinResults (dealerHand world) (nextPlayerHand !! 0))



nextEvent :: Float -> World -> World
nextEvent _ world = world

startGUI :: IO ()
startGUI = do
  let gameHeight = 600
  oneDeck <- getShuffledDecks 1
  twoDeck <- getShuffledDecks 2
  threeDeck <- getShuffledDecks 3
  fourDeck <- getShuffledDecks 4
  fiveDeck <- getShuffledDecks 5
  sixDeck <- getShuffledDecks 6
  let startWorld = World [EmptyDeck] EmptyDeck EmptyDeck EmptyDeck True "" (False,False) True [oneDeck,twoDeck,threeDeck,fourDeck,fiveDeck,sixDeck] False
  play (InWindow "Black Jack" (800, gameHeight) (10,10)) darkTeal 1 startWorld setWorld handleEvent nextEvent

cardPictures :: Bool -> Float -> Deck -> [Picture]
cardPictures _ _ EmptyDeck = []
cardPictures _ _ (Deck []) = []
cardPictures True height (Deck (c:_)) = (getCardImage c 2 height) : [(hiddenCardPicture height (getMinWidth 1))]
cardPictures hiddenDealer height (Deck (c:cs)) = getCardImage c (length (c:cs)) height : cardPictures hiddenDealer height (Deck cs)

getCardImage :: Card -> Int -> Float -> Picture
getCardImage (Card s r) numCards height = case s of
  Diamond -> cardPicture diamondPicture height (getMinWidth numCards) (Card s r) boldColor
  Heart   -> cardPicture heartPicture height (getMinWidth numCards) (Card s r) boldColor
  Spade   -> cardPicture spadePicture height (getMinWidth numCards) (Card s r) darkColor
  Club    -> cardPicture clubPicture height (getMinWidth numCards) (Card s r) darkColor

getMinWidth :: Int -> Float
getMinWidth 1        = (-35)
getMinWidth numCards = (-80) + getMinWidth (numCards - 1)

cardPicture :: (Float -> Float -> Picture) -> Float -> Float -> Card -> Color -> Picture
cardPicture f maxH minW (Card _ r) indColor
  | (show r) == "10" = pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , f maxH (minW)
                                                            , boldRankPicture indColor maxH minW (show r)
                                                            , f (maxH - 80) (minW + 54)
                                                            , boldRankPicture indColor (maxH - 80) (minW + 16) (show r)
                                                            ]
  | otherwise = pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , f maxH (minW)
                                                            , boldRankPicture indColor maxH minW (show r)
                                                            , f (maxH - 80) (minW + 54)
                                                            , boldRankPicture indColor (maxH - 80) (minW + 25) (show r)
                                                            ]


diamondPicture :: Float -> Float -> Picture
diamondPicture maxHeight minWidth = color boldColor $ polygon [((minWidth + 8),(maxHeight - 1))
                                                        , ((minWidth + 15),(maxHeight - 10))
                                                        , ((minWidth + 8),(maxHeight - 19))
                                                        , ((minWidth + 1),(maxHeight - 10))
                                                        ]

heartPicture :: Float -> Float -> Picture
heartPicture maxHeight minWidth = color boldColor $ polygon [((minWidth + 8),(maxHeight - 7))
                                                            ,((minWidth + 10),(maxHeight - 3))
                                                            ,((minWidth + 12),(maxHeight - 3))
                                                            ,((minWidth + 14),(maxHeight - 4))
                                                            ,((minWidth + 15),(maxHeight - 6))
                                                            ,((minWidth + 15),(maxHeight - 8))
                                                            ,((minWidth + 14),(maxHeight - 10))
                                                            ,((minWidth + 8),(maxHeight - 17))
                                                            ,((minWidth + 2),(maxHeight - 10))
                                                            ,((minWidth + 1),(maxHeight - 8))
                                                            ,((minWidth + 1),(maxHeight - 6))
                                                            ,((minWidth + 2),(maxHeight - 4))
                                                            ,((minWidth + 4),(maxHeight - 3))
                                                            ,((minWidth + 6),(maxHeight - 3))
                                                            ]

spadePicture :: Float -> Float -> Picture
spadePicture maxHeight minWidth = color darkColor $ polygon [((minWidth + 8),(maxHeight - 1))
                                                            ,((minWidth + 14),(maxHeight - 9))
                                                            ,((minWidth + 15),(maxHeight - 11))
                                                            ,((minWidth + 15),(maxHeight - 13))
                                                            ,((minWidth + 14),(maxHeight - 15))
                                                            ,((minWidth + 13),(maxHeight - 16))
                                                            ,((minWidth + 11),(maxHeight - 16))
                                                            ,((minWidth + 9),(maxHeight - 13))
                                                            ,((minWidth + 9),(maxHeight - 16))
                                                            ,((minWidth + 10),(maxHeight - 18))
                                                            ,((minWidth + 11),(maxHeight - 19))
                                                            ,((minWidth + 5),(maxHeight - 19))
                                                            ,((minWidth + 6),(maxHeight - 18))
                                                            ,((minWidth + 7),(maxHeight - 16))
                                                            ,((minWidth + 7),(maxHeight - 13))
                                                            ,((minWidth + 5),(maxHeight - 16))
                                                            ,((minWidth + 3),(maxHeight - 16))
                                                            ,((minWidth + 2),(maxHeight - 15))
                                                            ,((minWidth + 1),(maxHeight - 13))
                                                            ,((minWidth + 1),(maxHeight - 11))
                                                            ,((minWidth + 2),(maxHeight - 9))
                                                            ]

clubPicture :: Float -> Float -> Picture
clubPicture maxHeight minWidth = color darkColor $ polygon [((minWidth + 9),(maxHeight - 1))
                                                            ,((minWidth + 11),(maxHeight - 2))
                                                            ,((minWidth + 12),(maxHeight - 4))
                                                            ,((minWidth + 12),(maxHeight - 5))
                                                            ,((minWidth + 11),(maxHeight - 7))
                                                            ,((minWidth + 9),(maxHeight - 9))
                                                            ,((minWidth + 12),(maxHeight - 8))
                                                            ,((minWidth + 14),(maxHeight - 9))
                                                            ,((minWidth + 15),(maxHeight - 11))
                                                            ,((minWidth + 15),(maxHeight - 12))
                                                            ,((minWidth + 14),(maxHeight - 14))
                                                            ,((minWidth + 11),(maxHeight - 15))
                                                            ,((minWidth + 9),(maxHeight - 14))
                                                            ,((minWidth + 8),(maxHeight - 12))
                                                            ,((minWidth + 9),(maxHeight - 15))
                                                            ,((minWidth + 11),(maxHeight - 19))
                                                            ,((minWidth + 5),(maxHeight - 19))
                                                            ,((minWidth + 7),(maxHeight - 15))
                                                            ,((minWidth + 8),(maxHeight - 12))
                                                            ,((minWidth + 7),(maxHeight - 14))
                                                            ,((minWidth + 5),(maxHeight - 15))
                                                            ,((minWidth + 2),(maxHeight - 14))
                                                            ,((minWidth + 1),(maxHeight - 12))
                                                            ,((minWidth + 1),(maxHeight - 11))
                                                            ,((minWidth + 2),(maxHeight - 9))
                                                            ,((minWidth + 4),(maxHeight - 8))
                                                            ,((minWidth + 7),(maxHeight - 9))
                                                            ,((minWidth + 5),(maxHeight - 7))
                                                            ,((minWidth + 4),(maxHeight - 5))
                                                            ,((minWidth + 4),(maxHeight - 4))
                                                            ,((minWidth + 5),(maxHeight - 2))
                                                            ,((minWidth + 7),(maxHeight - 1))
                                                            ]

hiddenCardPicture :: Float -> Float -> Picture
hiddenCardPicture maxH minW = pictures [color cardColor $ polygon [(minW,maxH)
                                                                  ,((minW + 70),maxH)
                                                                  ,((minW + 70),(maxH - 100))
                                                                  ,(minW,(maxH - 100))]
                                        , color boldColor $ polygon [((minW + 10),(maxH - 10))
                                                                    , ((minW + 60),(maxH - 10))
                                                                    , ((minW + 35),(maxH - 50))]
                                        , color darkColor $ polygon [((minW + 35),(maxH - 50))
                                                                    ,((minW + 60),(maxH - 90))
                                                                    ,((minW + 10),(maxH - 90))]]

boldRankPicture :: Color -> Float -> Float -> String -> Picture
boldRankPicture itemC maxH minW rank = color itemC $ translate (minW + 16) (maxH - 18) $ scale (0.15) (0.15) $ text rank

-- Picture Representation of banner "BLACKJACK"
titlePicture :: Picture
titlePicture = pictures [ color boldColor $ translate (-175) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        , color boldColor $ translate (-176) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        , color boldColor $ translate (-174) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        , color boldColor $ translate (-175) (201) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        , color boldColor $ translate (-175) (199) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        , color boldColor $ translate (-175) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                        ]

nextCardButton :: Picture
nextCardButton = pictures [ color darkColor $ polygon [(100,50),(300,50),(300,0),(100,0)]
                          , color boldColor $ translate (120) (10) $ scale (0.25) (0.25) $ text "Next Card"
                          , color boldColor $ translate (119) (10) $ scale (0.25) (0.25) $ text "Next Card"
                          , color boldColor $ translate (121) (10) $ scale (0.25) (0.25) $ text "Next Card"
                          , color boldColor $ translate (120) (11) $ scale (0.25) (0.25) $ text "Next Card"
                          , color boldColor $ translate (120) (9)  $ scale (0.25) (0.25) $ text "Next Card"
                          ]


hitMeButton :: Picture
hitMeButton = pictures [color boldColor $ polygon [(100,100),(300,100),(300,50),(100,50)]
                        , color darkColor $ translate (145) (60) $ scale (0.25) (0.25) $ text "Hit Me"]

stayButton :: Picture
stayButton = pictures [color boldColor $ polygon [(100,0),(300,0),(300,-50),(100,-50)]
                      , color darkColor $ translate (170) (-35) $ scale (0.25) (0.25) $ text "Stay"]

splitButton :: Picture
splitButton = pictures [color boldColor $ polygon [(100,-100),(300,-100),(300,-150),(100,-150)]
                        , color darkColor $ translate (120) (-135) $ scale (0.25) (0.25) $ text "Split Deck"]

numDecksWindow :: Picture
numDecksWindow = pictures [numDecksLabel
                          , oneDeckButton
                          , twoDeckButton
                          , threeDeckButton
                          , fourDeckButton
                          , fiveDeckButton
                          , sixDeckButton]

numDecksLabel :: Picture
numDecksLabel = color darkColor $ translate (-200) (100) $ scale (0.25) (0.25) $ text "Select Number of Decks"

oneDeckButton :: Picture
oneDeckButton = pictures [color boldColor $ polygon [(-170,0),(-130,0),(-130,-40),(-170,-40)]
                          , color darkColor $ translate (-158) (-32) $ scale (0.25) (0.25) $ text "1"]

twoDeckButton :: Picture
twoDeckButton = pictures [color boldColor $ polygon [(-110,0),(-70,0),(-70,-40),(-110,-40)]
                          , color darkColor $ translate (-98) (-32) $ scale (0.25) (0.25) $ text "2"]

threeDeckButton :: Picture
threeDeckButton = pictures [color boldColor $ polygon [(-50,0),(-10,0),(-10,-40),(-50,-40)]
                          , color darkColor $ translate (-38) (-32) $ scale (0.25) (0.25) $ text "3"]

fourDeckButton :: Picture
fourDeckButton = pictures [color boldColor $ polygon [(10,0),(50,0),(50,-40),(10,-40)]
                          , color darkColor $ translate (22) (-32) $ scale (0.25) (0.25) $ text "4"]

fiveDeckButton :: Picture
fiveDeckButton = pictures [color boldColor $ polygon [(70,0),(110,0),(110,-40),(70,-40)]
                          , color darkColor $ translate (82) (-32) $ scale (0.25) (0.25) $ text "5"]

sixDeckButton :: Picture
sixDeckButton = pictures [color boldColor $ polygon [(130,0),(170,0),(170,-40),(130,-40)]
                          , color darkColor $ translate (142) (-32) $ scale (0.25) (0.25) $ text "6"]

resultsPanel :: String -> (Bool,Bool) -> Picture
resultsPanel resultM (playerBlackJack,dealerBlackJack)
  | playerBlackJack == True && dealerBlackJack == True = pictures [color darkColor $ translate (-350) (-200) $ scale (0.25) (0.25) $ text "You both got BLACKJACK"
                                                                  , results]
  | playerBlackJack == True = pictures [color darkColor $ translate (-350) (-200) $ scale (0.25) (0.25) $ text "!!!BLACKJACK!!!"
                                        , results]
  | dealerBlackJack == True = pictures [color darkColor $ translate (-350) (-200) $ scale (0.25) (0.25) $ text "Dealer Has BLACKJACK"
                                        , results]
  | otherwise = results
    where
      results = color darkColor $ translate (-350) (-250) $ scale (0.25) (0.25) $ text resultM



dealerLabel :: Picture
dealerLabel = color darkColor $ translate (-150) (150) $ scale (0.25) (0.25) $ text "Dealer Hand"

playerLabel :: Picture
playerLabel = color boldColor $ translate (-150) (-40) $ scale (0.25) (0.25) $ text "Player Hand"

darkTeal :: Color
darkTeal = makeColor 0.0863 0.627 0.522 1

boldColor :: Color
boldColor = makeColor 0.729 0.188 0.357 1

darkColor :: Color
darkColor = makeColor 0.09 0.02 0.04 1

cardColor :: Color
cardColor = makeColor 0.90 0.81 0.620 1
