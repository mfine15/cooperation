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


main =  getArgs >>= parse {--do
      a <- getStdGen
      output "Length agents" (length $ agent a)
      output "OtherLength agnets" (length $ nub $ positions $ agent a)
      --G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (render 400 (History []  (agent (mkStdGen 8))))
      G.simulate (G.InWindow "My Window" (400, 400) (0,0)) G.white 2 (playRound (agent a) 2) (render 400) (greproduce 2)
      putStrLn $ show $ nub $  maxer a





    where agent a = generate a 16
          len sim = map (length . ints) sim
          sim a = take 500 $ simulate 40 (agent a)
          maxer a = map (foldr1 (maxTuple)) (map (showSums) (sim a))
          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs --}
