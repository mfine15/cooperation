
titForTat :: [(Bool,Bool)] -> Bool
titForTat history = fst $ head history

grim :: [(Bool,Bool)] -> Bool
grim history
    | null history = True
    | not (fst $ head history) = False
    | not (snd $ head history) = False
    | otherwise = True

sucker :: [(Bool,Bool)] -> Bool
sucker history = True

play :: [(Bool,Bool)] -> ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)]
play history agent1 agent2  = (agent1 history,agent2 history) : history

playAll :: [(Bool,Bool)] -> ([(Bool,Bool)] -> Bool) -> ([(Bool,Bool)] -> Bool) -> [(Bool,Bool)]
playAll history agent1 agent2 = playAll (play history agent1 agent2) agent1 agent2

score :: (Bool,Bool) -> (Int,Int)
score (True,False ) = (0,4)
score (False,True ) = (4,0)
score (True,True  ) = (2,2)
score (False,False) = (1,1)

tally :: [(Bool,Bool)] -> (Int,Int)
tally history = foldr1 sumTuple (map score history)


sumTuple :: (Num a) => (a,a) -> (a,a) -> (a,a)
sumTuple (a,b) (x,y) = (a+x,b+y)