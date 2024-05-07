module GUI where

import           CardDeckMAC
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           RegBlackJackGameMAC
import           ShuffleDeck


data World = World {playerHand      :: Deck
                    , dealerHand    :: Deck
                    , discPile      :: Deck
                    , playDeck      :: Deck
                    , playerTurn    :: Bool
                    , resultMessage :: String
                    }

setWorld :: World -> Picture
setWorld world
  | playerTurn world = pictures ([titlePicture, dealerLabel, playerLabel, hitMeButton, stayButton]
                                ++ (cardPictures 120 (dealerHand world))
                                ++ (cardPictures (-60) (playerHand world)))
  | otherwise = pictures ([titlePicture, dealerLabel, playerLabel, nextCardButton, (resultsPanel (resultMessage world))]
                          ++ (cardPictures 120 (dealerHand world))
                          ++ (cardPictures (-60) (playerHand world)))

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) world
  | not (playerTurn world) =
    if (x >= 100 && x <= 300 && y >= 0 && y <= 50) then do
      getNextHand world
    else world
  | otherwise =
    if (x >= 100 && x <= 300 && y >= 50 && y <= 100) then hitPlayer world
    else if (x >= 100 && x <= 300 && y >= (-100) && y <= (-50)) then showResults world
    else world
handleEvent _ world = world

hitPlayer :: World -> World
hitPlayer world =
  if (canPlayerPlay (playerHand world)) then
    if (canPlayerPlay (playerHNew)) then world {playerHand = playerHNew
                                                    , discPile = discPNew
                                                    , playDeck = playDNew}
    else showResults (world {playerHand = playerHNew
                                                    , discPile = discPNew
                                                    , playDeck = playDNew})
  else showResults world
    where
      (playerHNew, discPNew, playDNew) = dealCard (playerHand world) (discPile world) (playDeck world)

showResults :: World -> World
showResults world = world {dealerHand = dealerHandNew
                          , discPile = discPileNew
                          , playDeck = playDeckNew
                          , playerTurn = False
                          , resultMessage = resultMessageNew}
  where
    (dealerHandNew, discPileNew, playDeckNew) = takeDealerTurn (dealerHand world) (discPile world) (playDeck world)
    resultMessageNew = show (determinResults (dealerHand world) (playerHand world))


getNextHand :: World -> World
getNextHand world = (world {playerHand = playerHandNew
              , dealerHand = dealerHandNew
              , discPile = discPileNew
              , playDeck = playDeckNew
              , playerTurn = True})
  where
    (playerHandNew, dealerHandNew, discPileNew, playDeckNew) = dealHand (playDeck world) (discPile world)


nextEvent :: Float -> World -> World
nextEvent _ world = world

startGUI :: IO ()
startGUI = do
  initialDeck <- (shuffleDeck createDeck)
  let shuffledDeck = initialDeck
  let (playerHandInit, dealerHandInit, discPileInit, playDeckInit) = dealHand shuffledDeck EmptyDeck
  let gameHeight = 600
  let startWorld = World playerHandInit dealerHandInit discPileInit playDeckInit True ""
  play (InWindow "Black Jack" (800, gameHeight) (10,10)) darkTeal 1 startWorld setWorld handleEvent nextEvent
  --display (InWindow "Black Jack" (800, gameHeight) (10, 10)) darkTeal (fullDisplayPicture dealerHand playerHand)

fullDisplayPicture :: Deck -> Deck -> Picture
fullDisplayPicture dHand pHand = pictures ([titlePicture, dealerLabel, playerLabel]
                                          ++ (cardPictures 120 dHand)
                                          ++ (cardPictures (-60) pHand))


cardPictures :: Float -> Deck -> [Picture]
cardPictures _ EmptyDeck = []
cardPictures _ (Deck []) = []
cardPictures height (Deck (c:cs)) = go c (length (c:cs)): cardPictures height (Deck cs)
  where
    go (Card s r) numCards = case s of
      Diamond -> diamondCardPicture height (getMinWidth numCards) (Card s r)
      Heart   -> heartCardPicture height (getMinWidth numCards) (Card s r)
      Spade   -> spadeCardPicture height (getMinWidth numCards) (Card s r)
      Club    -> clubCardPicture height (getMinWidth numCards) (Card s r)


getMinWidth :: Int -> Float
getMinWidth 1        = (-35)
getMinWidth numCards = (-80) + getMinWidth (numCards - 1)


diamondCardPicture :: Float -> Float -> Card -> Picture
diamondCardPicture maxH minW (Card _ r)= pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , diamondPicture maxH (minW)
                                                            , boldRankPicture boldColor maxH minW (show r)
                                                            , diamondPicture (maxH - 80) (minW + 54)
                                                            , boldRankPicture boldColor (maxH - 80) (minW + 26) (show r)
                                                            ]

heartCardPicture :: Float -> Float -> Card -> Picture
heartCardPicture maxH minW (Card _ r)= pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , heartPicture maxH (minW)
                                                            , boldRankPicture boldColor maxH minW (show r)
                                                            , heartPicture (maxH - 80) (minW + 54)
                                                            , boldRankPicture boldColor (maxH - 80) (minW + 26) (show r)
                                                            ]

spadeCardPicture :: Float -> Float -> Card -> Picture
spadeCardPicture maxH minW (Card _ r)= pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , spadePicture maxH (minW)
                                                            , boldRankPicture darkColor maxH minW (show r)
                                                            , spadePicture (maxH - 80) (minW + 54)
                                                            , boldRankPicture darkColor (maxH - 80) (minW + 26) (show r)
                                                            ]

clubCardPicture :: Float -> Float -> Card -> Picture
clubCardPicture maxH minW (Card _ r)= pictures [color cardColor $ polygon [(minW,maxH)
                                                                                      ,((minW + 70),maxH)
                                                                                      ,((minW + 70),(maxH - 100))
                                                                                      ,(minW,(maxH - 100))
                                                                                      ]
                                                            , clubPicture maxH (minW)
                                                            , boldRankPicture darkColor maxH minW (show r)
                                                            , clubPicture (maxH - 80) (minW + 54)
                                                            , boldRankPicture darkColor (maxH - 80) (minW + 26) (show r)
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

boldRankPicture :: Color -> Float -> Float -> String -> Picture
boldRankPicture itemC maxH minW rank
  | rank == "10" = color itemC $ translate (minW + 22) (maxH - 18) $ scale (0.15) (0.15) $ text rank
  | otherwise = color itemC $ translate (minW + 16) (maxH - 18) $ scale (0.15) (0.15) $ text rank

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
nextCardButton = pictures [color boldColor $ polygon [(100,50),(300,50),(300,0),(100,0)]
                            , color darkColor $ translate (100) (5) $ scale (0.25) (0.25) $ text "Next Card"]

hitMeButton :: Picture
hitMeButton = pictures [color boldColor $ polygon [(100,100),(300,100),(300,50),(100,50)]
                        , color darkColor $ translate (145) (60) $ scale (0.25) (0.25) $ text "Hit Me"]

stayButton :: Picture
stayButton = pictures [color boldColor $ polygon [(100,-50),(300,-50),(300,-100),(100,-100)]
                      , color darkColor $ translate (170) (-85) $ scale (0.25) (0.25) $ text "Stay"]

resultsPanel :: String -> Picture
resultsPanel resultM = color darkColor $ translate (-350) (-250) $ scale (0.25) (0.25) $ text resultM

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
