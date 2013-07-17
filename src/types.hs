
module Types
where

import qualified Text.Show.Pretty as PP
import Data.Map


data History = History {
                        ints::[Interaction],
                        agents::[Agent]
                       } deriving (Eq)
instance Show History where
  show = PP.ppShow

data Agent = Agent {
                    function::[(Bool,Bool)] -> Bool,
                    name::String,
                    position::(Int,Int),
                    dna::DNA,
                    token::Int
                   }
instance Show Agent where
    show a = (show $ token a) ++ name a
-- Trying to make this faster
instance Eq Agent where
    (==) a1 a2 = (token a1) == (token a2)
-- just so you can have a map of agents
instance Ord Agent where
    compare a1 a2 = (uncurry (*) (position a1)) `compare` (uncurry (*) (position a2))
data Interaction = Interaction{
                                a1::Agent,
                                a2::Agent,
                                hist::[(Bool,Bool)]
                              }deriving (Show)
instance Eq Interaction where
    (==) i1 i2 = agents && (hist i1 == hist i2)
        where agents = (a1 i1,a2 i1) == (a1 i2,a2 i1)

type DNA = [(Gene,Float)]


data Gene = Gene {
                  title :: String,
                  fn    :: [(Bool,Bool)] -> Bool
                 }
instance Show Gene where
    show = title
instance Eq Gene where
    (==) g1 g2 = title g1 == title g2

data Stats = Stats {
                    rankings :: [Agent]
                  , iterations :: Int
                  , interactions :: Int
                  {--, defections :: Map Agent Int
                  , cooperations :: Map Agent Int
                  , fitness :: Map Agent Int --}
                  } deriving (Eq,Show)
