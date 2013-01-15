import Agent




play :: ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)] ->  [(Bool,Bool)]
play a1 a2 history  = (a1 history, a2 history) : play a1 a2 ((a1 history, a2 history):history)

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
