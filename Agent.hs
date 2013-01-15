module Agent
(pavlov,
 titForTat,
 sucker,
 grim,
) where

pavlov :: [(Bool,Bool)] -> Bool
pavlov history
    | tuple == (True,True) = True
    | tuple == (True,False) = True
    | tuple == (False,True) = False
    | tuple == (False,False) = True
    | otherwise = True
    where tuple = head history


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
