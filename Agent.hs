module Agent
(
 pavlov,
 titForTat,
 sucker,
 grim,
 defector,
 mistrusting
) where



pavlov :: [(Bool,Bool)] -> Bool
pavlov history
    | null history = True
    | tuple == (True,True) = True
    | tuple == (True,False) = True
    | tuple == (False,True) = False
    | tuple == (False,False) = True
    | otherwise = True
    where tuple = head history

sucker :: [(Bool,Bool)] -> Bool
sucker history = True

defector :: [(Bool,Bool)] -> Bool
defector history = False

mistrusting :: [(Bool,Bool)] -> Bool
mistrusting history
    | null history = False
    | otherwise = fst $ head history

grim :: [(Bool,Bool)] -> Bool
grim history
    | null history = True
    | not (fst $ head history) = False
    | not (snd $ head history) = False
    | otherwise = True

titForTat :: [(Bool,Bool)] -> Bool
titForTat history
    | null history = True
    | otherwise = fst $ head history

