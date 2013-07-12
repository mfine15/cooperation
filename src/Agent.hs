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
import Types


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
