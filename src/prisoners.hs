import Agent
import qualified Data.List.Split as S
import System.Random
import Data.List
import Helpers
import Graphics
import Data.Maybe
import Debug.Trace
import qualified Graphics.Gloss as Gloss

data Interaction = Interaction Agent Agent [(Bool,Bool)] deriving (Show)

play :: Agent -> Agent -> [(Bool,Bool)] -> [(Bool,Bool)]
-- reverses tuples so the specifics agent's iteration is always first
play a b hist = (a1 rev, a2 hist) : play a b ((a1 rev, a2 hist):hist)
    where rev = reverseTuples hist
          a1 = function a
          a2 = function b

playRound :: [Agent] -> Int -> [Interaction]
playRound agents iterations  = map (makeInteract iterations) (match agents)


-- Too long for a lambda function
makeInteract :: Int -> (Agent,Agent) -> Interaction
makeInteract iterations (x,y) = Interaction x y (take iterations (play x y []))

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

showSums :: [Interaction] -> [(String,Int)]
showSums history = map (\a -> (name a,sumAgent history a)) agents
    where agents = nub  (map (\(Interaction a1 a2 _ ) ->  a2) history)

makeAgent :: Int -> Agent -> [Agent] -> Agent
makeAgent generation a agents = Agent func (n++show empty) empty generation
    -- getgrid returns a tuple, but we assume that the grid is square
    where empty = head $ getEmpty (positions agents) (fst $ getGrid agents)
          func = function a
          -- remove previous agents position
          n =  head $ S.splitOn "(" (name a)

baseline :: [Interaction] -> Int
baseline int = sorted!!(round $ len/2)
  where sorted = sort sums
        sums = map snd (showSums int)
        agents = nub  $ map (\(Interaction a1 a2 _ ) ->  a2) int
        len  = fromIntegral $ length agents

{--
  This is a complicated function, made even moreso complicated for speed. You
  pass in the baseline, becuase it remains constant throughout the recursion. It
  will then extract the agents out from the interaction, and then reproduce an
  agent from the first winner, and append that on to the function called with
  the tail. We also don't want the winners recaluclated each time, so we will
  pass those to the function.
--}
new :: [Agent] -> Int -> [Agent] -> [Agent]
new [] _ _  = []
new winners base agents
      | not (null winners) = newAgent:new (tail winners) base agents
  where winner:_ = winners
        newAgent = makeAgent (generation winner + 1) winner agents

reproduce :: [Interaction] -> [Agent]
-- we may end up with too many due to agents with equal fitness
reproduce int = winners ++ take needed (new winners base agents)
    where base    = baseline int
          agents  = nub $ concat $ map (\(Interaction a1 a2 _ ) -> [a1,a2]) int
          winners = [a | a <- agents, sumAgent int a >= base]
          needed = (length agents - length winners)

main = do
  output "Length" (length int)
  output "Baseline" base
  output "Agents" (length agents)
  output "Interaction" int
  output "Sums" (showSums int)
  output "Winners" (length winners)
  output "equal" (length $ filter (\a -> snd a == base) (showSums int))
  output "New Agents" (length $ reproduce int)
  Gloss.display (Gloss.InWindow "My Window" (400, 400) (0,0)) Gloss.white (render agents 400)

    where agents = generate 9
          int = playRound agents 1
          base = baseline int
          winners = [a | a <- agents, sumAgent int a >= base]



