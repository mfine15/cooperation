import Agent
import Data.List


data Agent = Agent ([(Bool,Bool)] -> Bool) String
instance Show Agent where
    show (Agent function name) = name

data Interaction = Interaction Agent Agent [(Bool,Bool)] deriving (Show)


play :: ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)] ->  [(Bool,Bool)]
play a1 a2 history  = (a1 $ reverseTuples  history, a2  history) : play a1 a2 ((a1 $ reverseTuples history, a2 history):history) --reverse tuples so your iterations are always first

playRound :: [Agent] -> Int -> [Interaction]
playRound agents iterations = map (makeInteract iterations) (match agents)

makeInteract :: Int -> (Agent,Agent) -> Interaction -- made not lambda because of snytax error
makeInteract iterations (x,y) = Interaction x y (take iterations (play (getFunction x) (getFunction y) []))

getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ ) = func

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs
match :: [a] -> [(a,a)]
match [] = []
match a  = zipWith toTuple (repeat (head a)) a ++ (match (tail a))

toTuple :: a -> a -> (a,a)
toTuple a b = (a,b)

score :: (Bool,Bool) -> (Int,Int)
score (True,False ) = (0,4)
score (False,True ) = (4,0)
score (True,True  ) = (2,2)
score (False,False) = (1,1)

tally :: [(Bool,Bool)] -> (Int,Int)
tally history = foldr1 sumTuple (map score history)

sumTuple :: (Num a) => (a,a) -> (a,a) -> (a,a)
sumTuple (a,b) (x,y) = (a+x,b+y)

main = do
            putStrLn $ show $ playRound [Agent sucker "sucker", Agent titForTat "titForTat",
                                        Agent pavlov "pavlov", Agent grim "grim", Agent mistrusting "mistrusting",
                                        Agent defector "defector", Agent defector "defector1", Agent defector "defector2"] 2
            putStrLn $ show $ playRound [Agent titForTat "tit", Agent defector "defector"] 4
            putStrLn $ show $ take 10 $ play titForTat defector [(True,True)]
