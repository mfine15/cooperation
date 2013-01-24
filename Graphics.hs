module Graphics(display)
where

import Graphics.Gloss
import Agent

display :: [Agent] -> Int -> Picture -- assume window to be square
display agents size =
    where step = floor $
