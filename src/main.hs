import Prisoners
import Agent
import Graphics
import Genetics
import Helpers
import Data.List
import Parser
import System.Environment

import qualified Graphics.Gloss as G


main =  print len





    where agents = generate 16

          sim = take 10 $ simulate 3 agents
          h = head sim
          l = last sim
          len = map (length) sim
          scores = map showSums sim
          snds = map (map snd) scores
          most = maximum q
          q = (map snd (showSums h))
          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs
