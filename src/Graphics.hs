module Graphics(render)
where

import Graphics.Gloss
import Agent
import Data.List
import Data.Maybe
import qualified Data.List.Split as S
import qualified Data.Map as Map

render :: [Agent] -> Int -> Picture -- assume window to be square
render agents size = Pictures $ map (drawAgent  (step `div` 2) colors step) agents
    where step = size*6 `div` (length agents)
          nubNames = nub $ map (getName . name) agents --ignore the first two letters of name
          colors = Map.fromList $ zipWith (\name color-> (name, color )) nubNames colorlist
          colorlist = [black,red,green,blue,yellow,cyan,magenta,rose,violet,azure,aquamarine,chartreuse,orange]

drawAgent :: Int -> Map.Map String Color -> Int -> Agent -> Picture
drawAgent size colors step (Agent _ name (x,y) _ ) =
    color aColor (Polygon [(posA,posB),(posA,negB),(negA,negB),(negA,posB)])
    where aColor = fromJust $ Map.lookup (getName name) colors
          a = x * step
          b = y * step
          posA = fromIntegral $ a+size
          negA = fromIntegral $ a-size
          posB = fromIntegral $ b+size
          negB = fromIntegral $ b-size

getName :: String -> String
getName a = head $ S.splitOn "(" (tail a)
