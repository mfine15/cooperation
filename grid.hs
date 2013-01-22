module Grid
(neighbour)
where

import Agent

neighbour :: Agent -> Agent -> Bool
neighbour (Agent _ _ (x,y)) (Agent _ _ (a,b) ) = (abs $ x-a) <= 1 || (abs $ y-b) <=1  --curently gets corners