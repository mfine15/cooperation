module Parser(parse)
where

import Prisoners
import Agent
import Graphics
import Genetics
import Helpers
import Data.List
import qualified Graphics.Gloss as G
import System.Environment
import System.Exit

parse :: [String] -> IO a
parse ["-h"] = putStrLn usage >> exitSuccess
parse ["playRound",n,agents] = putStrLn (show $ playRound (generate (toInt agents)) (toInt n) ) >> exitSuccess
parse ["display",n] = (G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white pic) >> exitSuccess
    where pic = render 400 $ playRound (generate (toInt n)) 1
parse ["sim",n,len] = putStrLn (show $ simulate size (generate agent)) >> exitSuccess
    where agent = toInt n
          size  = toInt len
parse [n] = putStrLn "Come again?" >> exitSuccess

toInt n = read n :: Int
usage = "A model of the evolution of altruism"