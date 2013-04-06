 module Agent
(Agent(..),
 Interaction(..),
 pavlov,
 titForTat,
 sucker,
 grim,
 defector,
 mistrusting,
 randomAgent
) where

import Genetics
import System.IO.Unsafe
import System.Random

data Agent = Agent {
                    function::[(Bool,Bool)] -> Bool,
                    name::String,
                    position::(Int,Int),
                    dna::DNA
                   }
instance Show Agent where
    show a = show $ name a
-- Trying to make this faster
instance Eq Agent where
    (==) a1 a2 = position a1 == position a2
-- just so you can have a map of agents
instance Ord Agent where
    compare a1 a2 = (uncurry (*) (position a1)) `compare` (uncurry (*) (position a2))
data Interaction = Interaction{
                                a1::Agent,
                                a2::Agent,
                                hist::[(Bool,Bool)]
                              }deriving (Show)


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
grim history = null history || (fst h && snd h)
    where h = head history

titForTat :: [(Bool,Bool)] -> Bool
titForTat history
    | null history = True
    | otherwise = fst $ head history

randomAgent :: [(Bool,Bool)] -> Bool
randomAgent history = (unsafePerformIO $ getStdRandom $ randomR (1,100) :: Int) > 50
