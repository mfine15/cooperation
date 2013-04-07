module Prisoners(History(..),
                 play,
                 playRound,
                 getHistory,
                 sumAgent,
                 score,
                 showSums,
                 baseline,
                 generate,
                 generate',
                 makeAgent,
                 reproduce,
                 new,
                 simulate)
where

import Agent
import qualified Data.List.Split as S
import System.Random
import Data.List
import Helpers
import Graphics
import Types
import Genetics
import Data.Maybe
import Debug.Trace
import System.IO
import qualified Graphics.Gloss as G


play :: Agent -> Agent -> [(Bool,Bool)] -> [(Bool,Bool)]
-- reverses tuples so the specifics agent's iteration is always first
play a b hist = (a1 rev, a2 hist) : play a b ((a1 rev, a2 hist):hist)
    where rev = reverseTuples hist
          a1 = function a
          a2 = function b

playRound :: [Agent] -> Int -> History
playRound agents iterations  = History (map (makeInteract iterations) (cat agents)) agents


-- Too long for a lambda function
makeInteract :: Int -> (Agent,Agent) -> Interaction
makeInteract iterations (x,y) = Interaction x y (take iterations (play x y []))

getHistory :: [Interaction] -> Agent -> [[(Bool,Bool)]]
getHistory ints agent = map (getInt agent) interactions
  where getInt agent (Interaction a1 a2 xs) = xs
        interactions = filter (\(Interaction a1 a2 _) ->
                              agent == a1 || agent == a2) ints
match :: [Agent] -> [(Agent,Agent)]
match xs = filter (uncurry neighbour) (cat xs)

sumAgent :: [Interaction] -> Agent -> Int
sumAgent xs agent = sum (map (sumInteraction agent) xs)

-- Use this in a map call of sumAgent to return the sums of a specific agent
sumInteraction :: Agent -> Interaction  -> Int
sumInteraction agent (Interaction a1 a2 xs )
    | (==) agent a1 = sum $ map fst scores
    | (==) agent a2 = sum $ map snd scores
    | otherwise = 0
    where scores = map score xs

score :: (Bool,Bool) -> (Int,Int)
score (True,False ) = (0,4)
score (False,True ) = (4,0)
score (True,True  ) = (2,2)
score (False,False) = (1,1)

showSums :: History -> [(String,Int)]
showSums (History history agents) = map (\a -> (name a,sumAgent history a)) agents



baseline :: History -> Int
baseline (History int agents) = sorted!!(round $ len/2)
  where sorted = sort sums
        sums = map snd (showSums (History int agents))
        len  = fromIntegral $ length agents

generate :: StdGen -> Int -> [Agent]
generate gen num  = take num $ zipWith makeOne (cat range) (cycle filteredGenes)
    where limit  = round $ ((sqrt $ fromIntegral num)-1)/2
          range = permute gen [(-limit)..(limit)]
          filteredGenes = filter ((>2) . length) (powerset staticGenes)

makeOne :: (Int,Int) -> [Gene] -> Agent
makeOne (x,y) genes =
    Agent{
            function   = compose dna,
            name       = n name (x,y),
            position   = (x,y),
            dna = dna
        }
    where n name (x,y) = name++"("++show x++","++show y++  ")"
          dna = map (\gene  -> (gene,unsafeRandom (0.0,1.0))) genes
          name = fst (max2 dna) ++ "-" ++ snd (max2 dna)

makeAgent :: (Agent,Agent) -> [Agent] -> [Agent] -> (Agent,Agent)
makeAgent (a1,a2) winners agents = (n1,n2)
    -- getgrid returns a tuple, but we assume that the grid is square
    where (e1:e2:empty) = getEmpty (positions winners) (fst $ getGrid agents)
          -- remove previous agents position
          n a =  head $ S.splitOn "(" (name a)
          d1  = breed (dna a1) (dna a2)
          d2  = breed (dna a1) (dna a2) --random values
          f1  = compose d1
          f2  = compose d2
          n1  = Agent f1 (n a1++show e1) e1 d1
          n2  = Agent f2 (n a2 ++ show e2) e2 d2
{--
  You pass in the baseline, becuase it remains constant throughout the recursion.
  It will then extract the agents out from the interaction, and then reproduce an
  agent from the first winner, and append that on to the function called with
  the tail. We also don't want the winners recaluclated each time, so we will
  pass those to the function.
--}
new :: [Agent] -> Int -> [Agent] -> [Agent]
new (a:[]) base agents = [Agent
                          (function a)
                          (head $ S.splitOn "(" $ name a)
                          (head $ getEmpty (positions [a])
                          (fst $ getGrid agents))
                          (dna a)]
new (w1:w2:[]) base agents = let (new1,new2) = makeAgent (w1,w2) [w1,w2] agents in [new1,new2]
new (w1:w2:xs) base agents = new1:new2:new xs base agents
  where (new1,new2) = makeAgent (w1,w2) (w1:w2:xs) agents

reproduce :: History -> [Agent]
-- we may end up with too many due to agents with equal fitness
reproduce (History int agents ) = winners ++ take needed (new winners base agents)
    where base    = baseline (History int agents)
         -- agents  = nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2]) int
          winners = [a | a <- agents, sumAgent int a >= base]
          needed = length agents - length winners

--reproduce function that takes a few extra parameters for use with Gloss
greproduce view step history = playRound (reproduce history) 1

simulate :: Int -> [Agent] -> [History]
simulate  len  agents = iteration:(simulate len newAgents)
            where newAgents = reproduce iteration
                  iteration = playRound agents len

generate' :: Int -> [Agent]
generate' num = take num $ zipWith3 (\func name (x,y) ->
    ((Agent func (name++"("++show x++","++show y++  ")") (x,y) [])))
    (cycle agents) (cycle names) (cat range)
    where limit = round $ ((sqrt $ fromIntegral num)-1)/2
          agents = [pavlov,titForTat,sucker,grim,defector,mistrusting,randomAgent]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting","randomAgent"]
          range = [(-limit)..(limit)]


