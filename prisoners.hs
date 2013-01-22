import Agent
import System.Random
import Data.List

data Agent = Agent ([(Bool,Bool)] -> Bool) String (Int,Int)
instance Show Agent where
    show (Agent function name pos) = name ++ show pos
instance Eq Agent where
    (==) (Agent _ a1 _) (Agent _ a2 _   ) = a1 == a2

data Interaction = Interaction Agent Agent [(Bool,Bool)] deriving (Show)


play :: ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)] ->  [(Bool,Bool)]
play a1 a2 history  = (a1 $ reverseTuples  history, a2  history) : play a1 a2 ((a1 $ reverseTuples history, a2 history):history) --reverse tuples so your iterations are always first

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


--Helpers
cat :: [a] -> [(a,a)]
cat [] = []
cat a = zipWith (\a b -> (a,b)) (repeat $ head a) a ++ (cat $ tail a)

getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ _) = func

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

generate :: Int -> [Agent]
generate num = take num $ zipWith3 (\func name (x,y) -> ((Agent func ((['a'..'z']!!(x-1)):'_':name) (x,y))))
                                                         (cycle agents) (cycle names) (cat [1..(ceiling $ sqrt $ fromIntegral num)])
    where agents = [pavlov,titForTat,sucker,grim,defector,mistrusting]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]



--Grid
neighbour :: Agent -> Agent -> Bool
neighbour (Agent _ _ (x,y)) (Agent _ _ (a,b) ) = (abs $ x-a) <= 1 || (abs $ y-b) <=1  --curently gets corners

main = do
            putStrLn $ show $ neighbour (Agent titForTat "tits" (2,2)) (Agent defector "defector" (0,0))
            putStrLn $ show $ showSums  $ playRound (generate 6) 50

