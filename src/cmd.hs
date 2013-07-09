module Parser(parse)
where

import Prisoners
import Agent
import Graphics
import Types
import Genetics
import Helpers
import Data.List
import qualified Graphics.Gloss as G
import System.Environment
import System.Exit
import System.Random

parse :: [String] -> IO a
parse ["-h"] = putStrLn usage >> exitSuccess
parse ["playRound",n,agents] = do
                                    a <- getStdGen
                                    putStrLn (show $ playRound a (generate a (toInt agents)) (toInt n) ) >> exitSuccess
parse ["display","sim",n,len] = do
        a <- getStdGen
        G.simulate (G.InWindow "My Window" (400, 400) (0,0)) G.white 2 (playRound a (agent a) 2) (render 400) (greproduce a 2) >> exitSuccess
     where agent a = generate a 16

parse ["display",n] = do
                    a <- getStdGen
                    (G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (pic a)) >> exitSuccess
    where pic a = render 400 (history a)
          history a = playRound a (generate a (toInt n)) 1
parse ["sim",n,len] = do
    a <- getStdGen
    putStrLn (show $ take (toInt n) $ simulate a size (generate a agent)) >> exitSuccess
    where agent = toInt n
          size  = toInt len
parse [n] = putStrLn "Come again?" >> exitSuccess
parse _  = putStrLn "Come again" >> exitSuccess

toInt n = read n :: Int
usage = "A model of the evolution of altruism"