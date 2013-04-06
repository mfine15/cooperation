import Prisoners
import Agent
import Graphics
import Genetics
import Helpers
import Data.List
import Parser
import System.Environment

import qualified Graphics.Gloss as G


main =  do
      G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (render 400 (playRound agents 1))





    where agents = generate 100

          sim = take 10 $ simulate 1 agents
          h = head sim
          l = last sim
          len = map (length . ints) sim
          scores = map showSums sim
          snds = map (map snd) scores
          most = maximum q
          q = (map snd (showSums h))
          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs
