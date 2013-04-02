import Agent
import qualified Data.List.Split as S
import System.Random
import Data.List
import Helpers
import Graphics
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

playRound :: [Agent] -> Int -> [Interaction]
playRound agents iterations  = map (makeInteract iterations) (match agents)


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

showSums :: [Interaction] -> [(String,Int)]
showSums history = map (\a -> (name a,sumAgent history a)) agents
    where agents = nub  (map (\(Interaction a1 a2 _ ) ->  a2) history)



baseline :: [Interaction] -> Int
baseline int = sorted!!(round $ len/2)
  where sorted = sort sums
        sums = map snd (showSums int)
        agents = nub  $ map (\(Interaction a1 a2 _ ) ->  a2) int
        len  = fromIntegral $ length agents

generate :: Int -> [Agent]
generate num  = take num $ zipWith3 makeOne (cycle names) (cat range) (permutations composables)
    where limit  = round $ ((sqrt $ fromIntegral num)-1)/2
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
          range = permute (mkStdGen $ unsafeRandom (0,100)) [(-limit)..(limit)]
          composables :: [(Gene,Float)]
          composables = map (\gene  -> (gene,unsafeRandom (0.0,1.0))) sortedGenes
          sortedGenes = permute (mkStdGen $ unsafeRandom (1,100)) staticGenes

makeOne :: String -> (Int,Int) -> [(Gene,Float)] -> Agent
makeOne name (x,y) dna =
    Agent{
            function   = compose dna,
            name       = n name (x,y),
            position   = (x,y),
            dna = dna
        }
    where
        n name (x,y) = name++"("++show x++","++show y++  ")"


makeAgent :: (Agent,Agent) -> [Agent] -> (Agent,Agent)
makeAgent (a1,a2) agents = (a1,a2)
    -- getgrid returns a tuple, but we assume that the grid is square
    where empty = head $ getEmpty (positions agents) (fst $ getGrid agents)
          -- remove previous agents position
          n a =  head $ S.splitOn "(" (name a)
          d1  = breed (dna a1) (dna a2)
          d2  = breed (dna a1) (dna a2)
          f1  = compose d1
          f2  = compose d2
          a1  = Agent f1 (n a1++show empty) empty d1
          a2  = Agent f2 (n a2 ++ show empty) empty d2
{--
  You pass in the baseline, becuase it remains constant throughout the recursion.
  It will then extract the agents out from the interaction, and then reproduce an
  agent from the first winner, and append that on to the function called with
  the tail. We also don't want the winners recaluclated each time, so we will
  pass those to the function.
--}
new :: [Agent] -> Int -> [Agent] -> [Agent]
new [] _ _  = []
new winners base agents = new1:new2:new (tail winners) base agents
  where winner = take 2 $ winners
        (new1,new2) = makeAgent (head winner,last winner) winners

reproduce :: [Interaction] -> [Agent]
-- we may end up with too many due to agents with equal fitness
reproduce int = winners ++ take needed (new winners base agents)
    where base    = baseline int
          agents  = nub $ concatMap (\(Interaction a1 a2 _ ) -> [a1,a2]) int
          winners = [a | a <- agents, sumAgent int a >= base]
          needed = (length agents - length winners)

--reproduce function that takes a few extra parameters for use with Gloss
greproduce view step int = playRound (reproduce int) 1

simulate ::  Int -> [Agent] -> [[Interaction]]
simulate len agents  = iteration:(simulate len newAgents)
    where iteration = playRound agents len
          newAgents = reproduce iteration



main = do
  output "Length" (length int)
  output "Baseline" base
  output "Agents" (length agents)
  output "Interaction" int
  output "Sums" (showSums int)
  output "Winners" (length winners)
  output "equal" (length $ filter (\a -> snd a == base) (showSums int))
  output "New Agents" (length $ reproduce int)
  output "Simulation" sim






    where agents = generate 4
          int = playRound agents 10
          base = baseline int
          winners = [a | a <- agents, sumAgent int a >= base]
          history = filter (not . null) $ map (getHistory int) agents
          sim = take 2 $ simulate 1 agents



