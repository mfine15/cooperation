module Graphics(render,event)
where

import Agent
import Types
import Graphics.Gloss
import Data.List
import Data.Maybe
import Prisoners
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace
import qualified Data.List.Split as S
import qualified Data.Map as Map

render ::  Int -> History -> Picture -- assume window to be square
render size (History int agents) =  trace (show $ length $ nub agents) Pictures $ map (drawAgent (step`div`2) colors step) agents
    where step = size*6 `div` (length agents)
          --agents = nub $ concat $ map (\(Interaction a1 a2 _ ) -> [a1,a2]) int
          nubNames = nub $ map (getName . name) agents --ignore the first two letters of name
          colors = Map.fromList $ zipWith (\name color-> (name, color)) nubNames (cycle colorlist)
          colorlist = [red,green,blue,yellow,cyan,magenta,rose,violet,azure,aquamarine,chartreuse,orange]
          singleAgent = (writeOne (head agents) (History int agents))

drawAgent :: Int -> Map.Map String Color -> Int -> Agent -> Picture
drawAgent size colors step agent =
    color aColor (Polygon [(posA,posB),(posA,negB),(negA,negB),(negA,posB)])
    where aColor = fromMaybe black $ Map.lookup (getName $ name agent ) colors
          a = (fst $ position agent) * step
          b = (snd $ position agent) * step
          posA = fromIntegral $ a+size
          negA = fromIntegral $ a-size
          posB = fromIntegral $ b+size
          negB = fromIntegral $ b-size

{-- event (EventMotion (x,y)) world = world --(x,y)
event (EventKey (MouseButton LeftButton) Up mods (x,y)) world = (x,y):world --}
event _                   world = world

getName :: String -> String
getName a = take 5 a

writeOne :: Agent -> History -> Picture
writeOne agent history = Text (show $ name agent ++ ": "++ (show $ score))
  where score = sumAgent (ints history) agent
