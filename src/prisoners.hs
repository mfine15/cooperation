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
                 greproduce,
                 new,
                 simulate)
where

import Agent
import qualified Data.List.Split as S
import System.Random
import Data.List
import Helpers
import Types
import Genetics
import Data.Maybe
import Debug.Trace
import System.IO
import qualified Graphics.Gloss as G


play :: Agent -> Agent -> [(Bool,Bool)] -> [(Bool,Bool)]
-- reverses tuples so the specifics agent's iteration is always first
play  a b hist = (a1 rev, a2 hist) : play a b ((a1 rev, a2 hist):hist)
    where rev = reverseTuples hist
          a1 = function a
          a2 = function b

playRound :: StdGen -> [Agent] -> Int -> History
playRound gen agents iterations  = History (map (makeInteract gen iterations) (match agents)) agents


-- Too long for a lambda function
makeInteract :: StdGen -> Int -> (Agent,Agent) -> Interaction
makeInteract gen iterations (x,y) = Interaction x y (take iterations (play x y []))

getHistory :: [Interaction] -> Agent -> [[(Bool,Bool)]]
getHistory ints agent = map (getInt agent) interactions
  where getInt agent (Interaction a1 a2 xs) = xs
        interactions = filter (\(Interaction a1 a2 _) ->
                              agent == a1 || agent == a2) ints
match :: [Agent] -> [(Agent,Agent)]
match xs = filter (uncurry neighbour) (cat xs)

sumAgent :: [Interaction] -> Agent -> Int
sumAgent xs agent = foldr (\x acc -> acc + sumInteraction agent x) 0 xs

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
generate gen num  = take num $ zipWith makeOne range (cycle filteredGenes)
    where limit  = round $ ((sqrt $ fromIntegral num)-1)/2
          range =  if (length $ cat ([(-limit)..(limit)])) /= num
                   then cat [-limit..(limit-1)]
                   else cat [-limit..limit]
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

makeAgent :: StdGen -> (Agent,Agent) -> [Agent] -> [Agent] -> (Agent,Agent)
makeAgent gen (a1,a2) winners agents = (n1,n2)
    -- getgrid returns a tuple, but we assume that the grid is square
    where e1 = nearest (position a1) (positions winners)
          e2 = nearest (position a2) ((positions winners )\\ [e1])
          -- remove previous agents position
          n a =  head $ S.splitOn "(" (name a)
          d1  = breed gen (dna a1) (dna a2)
          d2  = breed gen (dna a1) (dna a2) --random values
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
new :: StdGen -> [Agent] -> Int -> [Agent] -> [Agent]
new gen winners base agents = flatten $ zipWith4 makeAgent (infiniteGen gen) (toTuple winners) (repeat winners) (repeat agents)


reproduce :: StdGen -> History -> [Agent]
-- we may end up with too many due to agents with equal fitness
reproduce gen (History int agents ) =  winners ++ take needed (new gen winners base agents)
    where base    = baseline (History int agents)
          winners = [a | a <- agents, sumAgent int a >= base]
          needed = length agents - length winners

--reproduce function that takes a few extra parameters for use with Gloss
greproduce gen len view step  history = playRound gen (reproduce gen history) len

simulate :: StdGen -> Int -> [Agent] -> [History]
simulate  gen len  agents =  (playRound gen agents len) : simulate newGen len (reproduce gen (playRound gen agents len))
  where (_,newGen) = randomR (1,110) gen :: (Int,StdGen)

generate' :: Int -> [Agent]
generate' num = take num $ zipWith3 (\func name (x,y) ->
    ((Agent func (name++"("++show x++","++show y++  ")") (x,y) [])))
    (cycle agents) (cycle names) (cat range)
    where limit = round $ ((sqrt $ fromIntegral num)-1)/2
          agents = [pavlov,titForTat,sucker,grim,defector,mistrusting,randomAgent]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting","randomAgent"]
          range = [(-limit)..(limit)]


