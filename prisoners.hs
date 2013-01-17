import Agent

import Data.List


data Agent = Agent ([(Bool,Bool)] -> Bool) String
instance Show Agent where
    show (Agent function name) = name
instance Eq Agent where
    (==) (Agent _ a1 ) (Agent _ a2 ) = a1 == a2

data Interaction = Interaction Agent Agent [(Bool,Bool)] deriving (Show)


play :: ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)] ->  [(Bool,Bool)]
play a1 a2 history  = (a1 $ reverseTuples  history, a2  history) : play a1 a2 ((a1 $ reverseTuples history, a2 history):history) --reverse tuples so your iterations are always first

playRound :: [Agent] -> Int -> [Interaction]
playRound agents iterations = map (makeInteract iterations) (match agents)

makeInteract :: Int -> (Agent,Agent) -> Interaction
makeInteract iterations (x,y) = Interaction x y (take iterations (play (getFunction x) (getFunction y) []))

match :: [a] -> [(a,a)]
match [] = []
match a  = zipWith (\a b -> (a,b)) (repeat (head a)) a ++ (match (tail a))

sumAgent :: Agent -> [Interaction] -> Int
sumAgent agent xs = foldr1 (+) (map (sumInteraction agent) xs)

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



getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ ) = func

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

main = do
            --putStrLn $ show $ (sumAgent (Agent titForTat "titForTat")) $ playRound [Agent sucker "sucker", Agent titForTat "titForTat",
            --                            Agent pavlov "pavlov", Agent grim "grim", Agent mistrusting "mistrusting",
            --                            Agent defector "defector", Agent defector "defector1", Agent defector "defector2"] 2
            putStrLn $ show $ sumAgent (Agent titForTat "tit") $ playRound [Agent titForTat "tit", Agent defector "defector"] 2
