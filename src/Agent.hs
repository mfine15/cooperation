 module Agent
(Agent(..),
 pavlov,
 titForTat,
 sucker,
 grim,
 defector,
 mistrusting
) where


data Agent = Agent ([(Bool,Bool)] -> Bool) String (Int,Int)
instance Show Agent where
    show (Agent function name pos) = name ++ show pos
instance Eq Agent where
    (==) (Agent _ a1 _) (Agent _ a2 _   ) = a1 == a2


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
grim history = if null history then True else fst h && snd h
    where h = head history

titForTat :: [(Bool,Bool)] -> Bool
titForTat history
    | null history = True
    | otherwise = fst $ head history

