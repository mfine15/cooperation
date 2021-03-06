module Types
where

data History = History {
                        ints::[Interaction],
                        agents::[Agent]
                       } deriving (Eq,Show)

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
    (==) a1 a2 = (position a1 == position a2) && (name a1 == name a2)
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