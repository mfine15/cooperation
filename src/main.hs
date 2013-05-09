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
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss as G


main = do
      a <- getStdGen
      output "Length agents" (length $ agent a)
      output "OtherLength agnets" (length $ nub $ positions $ agent a)
      --G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (render 400 (History []  (agent (mkStdGen 8))))
      --G.simulate (G.InWindow "My Window" (400, 400) (0,0)) G.white 2 (playRound (agent a) 2) (render 400) (greproduce 2)
      --G.play (G.InWindow "Window" (800,800) (0,0)) G.white  1 (head $ sim a) (render 800) event (step a)
      putStrLn $ show $  step a 1.0 (head $ sim a)





    where agent gen = generate gen 100
          len sim = map (length . ints) sim
          sim gen = simulate gen 2 (agent gen)
          maxer gen = map (foldr1 (maxTuple)) (map (showSums) (sim gen))
          step gen time world =  (simulate gen 5 (agent gen))!!(trace ("time"++(show $ round time)) round time)
          --as xs = map (nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2])) xs --}
