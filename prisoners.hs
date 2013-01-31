import Agent
import System.Random
import Data.List
import Helpers
import Grid
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


main = do
            putStrLn $ show $ neighbour (Agent titForTat "tits" (2,2)) (Agent defector "defector" (0,0))
            putStrLn $ show $ showSums  $ playRound (generate 6) 50

            Gloss.display (Gloss.InWindow "My Window" (200, 200) (10, 10)) Gloss.white (render (generate 64) 200)

