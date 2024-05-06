module GUI where

import           CardDeckMAC
import           Graphics.Gloss


startGUI :: IO ()
startGUI = do
  let playDeck = createDeck
  let gameHeight = 600
  display (InWindow "Black Jack" (800, gameHeight) (10, 10)) darkTeal (fullDisplayPicture playDeck)

fullDisplayPicture :: Deck -> Picture
fullDisplayPicture playDeck = pictures ([titlePicture] ++ (cardPictures 150 (takeXCards playDeck 3)))


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
diamondCardPicture maxHeight minWidth (Card _ r)= pictures [color cardColor $ polygon [(minWidth,maxHeight)
                                                                                      ,((minWidth + 70),maxHeight)
                                                                                      ,((minWidth + 70),(maxHeight - 100))
                                                                                      ,(minWidth,(maxHeight - 100))
                                                                                      ]
                                                            , diamondPicture maxHeight (minWidth)
                                                            , boldRankPicture boldColor maxHeight minWidth (show r)
                                                            , diamondPicture (maxHeight - 80) (minWidth + 54)
                                                            , boldRankPicture boldColor (maxHeight - 80) (minWidth + 26) (show r)
                                                            ]

heartCardPicture :: Float -> Float -> Card -> Picture
heartCardPicture maxHeight minWidth (Card _ r)= pictures [color cardColor $ polygon [(minWidth,maxHeight)
                                                                                      ,((minWidth + 70),maxHeight)
                                                                                      ,((minWidth + 70),(maxHeight - 100))
                                                                                      ,(minWidth,(maxHeight - 100))
                                                                                      ]
                                                            , heartPicture maxHeight (minWidth)
                                                            , boldRankPicture boldColor maxHeight minWidth (show r)
                                                            , heartPicture (maxHeight - 80) (minWidth + 54)
                                                            , boldRankPicture boldColor (maxHeight - 80) (minWidth + 26) (show r)
                                                            ]

spadeCardPicture :: Float -> Float -> Card -> Picture
spadeCardPicture maxHeight minWidth (Card _ r)= pictures [color cardColor $ polygon [(minWidth,maxHeight)
                                                                                      ,((minWidth + 70),maxHeight)
                                                                                      ,((minWidth + 70),(maxHeight - 100))
                                                                                      ,(minWidth,(maxHeight - 100))
                                                                                      ]
                                                            , spadePicture maxHeight (minWidth)
                                                            , boldRankPicture darkColor maxHeight minWidth (show r)
                                                            , spadePicture (maxHeight - 80) (minWidth + 54)
                                                            , boldRankPicture darkColor (maxHeight - 80) (minWidth + 26) (show r)
                                                            ]

clubCardPicture :: Float -> Float -> Card -> Picture
clubCardPicture maxHeight minWidth (Card _ r)= pictures [color cardColor $ polygon [(minWidth,maxHeight)
                                                                                      ,((minWidth + 70),maxHeight)
                                                                                      ,((minWidth + 70),(maxHeight - 100))
                                                                                      ,(minWidth,(maxHeight - 100))
                                                                                      ]
                                                            , clubPicture maxHeight (minWidth)
                                                            , boldRankPicture darkColor maxHeight minWidth (show r)
                                                            , clubPicture (maxHeight - 80) (minWidth + 54)
                                                            , boldRankPicture darkColor (maxHeight - 80) (minWidth + 26) (show r)
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
boldRankPicture itemColor maxHeight minWidth rank = color itemColor $ translate (minWidth + 16) (maxHeight - 18) $ scale (0.15) (0.15) $ text rank

-- Picture Representation of banner "BLACKJACK"
titlePicture :: Picture
titlePicture = pictures [ color boldColor $ translate (-175) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     , color boldColor $ translate (-176) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     , color boldColor $ translate (-174) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     , color boldColor $ translate (-175) (201) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     , color boldColor $ translate (-175) (199) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     , color boldColor $ translate (-175) (200) $ scale (0.5) (0.5) $ text "BLACKJACK"
                     ]

darkTeal :: Color
darkTeal = makeColor 0.0863 0.627 0.522 1

boldColor :: Color
boldColor = makeColor 0.729 0.188 0.357 1

darkColor :: Color
darkColor = makeColor 0.09 0.02 0.04 1

cardColor :: Color
cardColor = makeColor 0.90 0.81 0.620 1
