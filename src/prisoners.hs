import Agent
import System.Random
import Data.List
import Helpers
import Graphics
import qualified Graphics.Gloss as Gloss

data Interaction = Interaction Agent Agent [(Bool,Bool)] deriving (Show)

play :: ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)] ->  [(Bool,Bool)]
play a1 a2 history  = (a1 yrotsih, a2 history) : play a1 a2 ((a1 yrotsih, a2 history):history) --reverse tuples so your iterations are always first
    where yrotsih = reverseTuples history

playRound :: [Agent] -> Int -> [Interaction]
playRound agents iterations = map (makeInteract iterations) (match agents)

-- Too long for a lambda function
makeInteract :: Int -> (Agent,Agent) -> Interaction
makeInteract iterations (x,y) = Interaction x y (take iterations (play (getFunction x) (getFunction y) []))

match :: [Agent] -> [(Agent,Agent)]
match xs = filter (\z -> neighbour (fst z) (snd z)) (cat xs)

sumAgent :: [Interaction] -> Agent -> Int
sumAgent xs agent  = foldr1 (+) (map (sumInteraction agent) xs)

-- Use this in a map call of sumAgent to return the sums of an agent in interaction
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
showSums history = map (\(Agent a name b) -> (name,sumAgent history (Agent a name b))) agents --a hack to get around asigning agent to a value
    where agents = nub  (map (\(Interaction a1 a2 _ ) ->  a2) history)

getBaseline :: [Interaction] -> Float
getBaseline history =  (fromIntegral $ sum scores)/(fromIntegral $ length scores)
    where scores = map snd (showSums history)


makeAgent :: Agent -> [Agent] -> Agent
makeAgent (Agent func n _) agents = (Agent func (n++(show $ length $ sameNames n agents)) empty) --appends number to name to differentiate agents
    where sameNames n agents = filter (findName n) agents
          findName n1 (Agent _ n2 _) = (slice 0 3 n1) == (slice 0 3 n2) --ignore the suffix
          empty = head $ getEmpty (positions agents) (fst $ getGrid agents) --getGrid returns a tuple, but currently assume to be a square

baseline :: [Interaction] -> Int
baseline int = foldr (+) (snd $ head $ showSums int) (map snd (showSums int))

reproduce :: Int -> [Interaction] -> [Agent]  --so baseline isn't recalulated every time
reproduce _ [] = []
reproduce baseline interaction = winners ++ newAgent:reproduce baseline (tail interaction)
    where agents = nub $ concat $ map (\(Interaction a1 a2 _ ) -> a1:a2:[]) interaction
          winners = filter ((\a -> (sumAgent interaction a) >= baseline )) agents
          newAgent = makeAgent (head winners) winners


main = do
    prefix "Agents" (generate 16)
    prefix "New Agents" (reproduce base int)
    where agents = generate 64
          int = playRound agents 20
          base = baseline int



