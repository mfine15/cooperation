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


{--makeAgent :: Agent -> [Agent] -> Agent
makeAgent (Agent func n _) agents = (Agent func (n++length $ sameNames n agents)
    where sameNames n agents = filter (findNames n) agents
          findName n1 (Agent _ n2 _) = (slice 0 3 n1) == (slice 0 3 n2) --ignore the suffix
--}

{--
reproduce :: [Interaction] -> [Agent]
reproduce history = winners ++ (map makeAge winners) ++ neutrals
    where agents = nub  (map (\(Interaction a1 a2 _ ) ->  a2) history)
          base = getBaseline history
          winners = filter base< history
          neutrals = filter base== history
--}

main = do
           --Gloss.display (Gloss.InWindow "My Window" (1000, 1000) (10, 10)) Gloss.white (render (generate 64) 1000)
           --print $ map (\(Agent _ _ (x,y)) -> (x,y)) (generate 64)
           print $ map (\ (x, y) -> (toInteger x, toInteger y)) (map (\(Agent _ _ (x,y)) -> (x,y)) (generate 16))



