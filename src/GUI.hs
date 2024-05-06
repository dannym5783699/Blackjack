module GUI where

import           Graphics.Gloss


startGUI :: IO ()
startGUI = display (InWindow "Black Jack" (600, 400) (10, 10)) darkTeal helloWorld

-- Define the function to draw "Hello World" in the center of the screen
helloWorld :: Picture
helloWorld = pictures [ color boldColor $ translate (-175) (100) $ scale (0.5) (0.5) $ text "Black Jack"
                     , color boldColor $ translate (-176) (100) $ scale (0.5) (0.5) $ text "Black Jack"
                     , color boldColor $ translate (-174) (100) $ scale (0.5) (0.5) $ text "Black Jack"
                     , color boldColor $ translate (-175) (101) $ scale (0.5) (0.5) $ text "Black Jack"
                     , color boldColor $ translate (-175) (99) $ scale (0.5) (0.5) $ text "Black Jack"
                     , color boldColor $ translate (-175) (100) $ scale (0.5) (0.5) $ text "Black Jack"
                     ]


darkTeal :: Color
darkTeal = makeColor 0.0863 0.627 0.522 1

boldColor :: Color
boldColor = makeColor 0.729 0.188 0.357 1
