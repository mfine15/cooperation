import Prisoners
import Agent
import Graphics
import Genetics
import Types
import Helpers
import Data.List
import Parser
import System.Environment
import System.Random
import Debug.Trace

import qualified Graphics.Gloss as G


main =  do
      a <- getStdGen
      G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (render 400 (History [] (trace (show $ length $ agents a) agents a)))
      --putStrLn (show $ length $ agents a)





    where agents a = generate a 529
          --len = map (length . ints) sim

          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs
