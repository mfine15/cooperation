import Prisoners
import Agent
import Graphics
import Genetics
import Helpers
import Data.List
import Parser
import System.Environment

import qualified Graphics.Gloss as G


main = getArgs >>= parse





    where agents = generate 225

          sim = take 300 $ simulate 3 agents
          h = head sim
          l = last sim
          len = map (length . ag) sim
          ag int = nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2]) int
          scores = map showSums sim
          snds = map (map snd) scores
          most = maximum q
          q = (map snd (showSums h))
          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs
