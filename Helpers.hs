module Helpers
(cat,
 getFunction,
 reverseTuples,
 generate
) where

import Agent

cat :: [a] -> [(a,a)]
cat [] = []
cat a = zipWith (\a b -> (a,b)) (repeat $ head a) a ++ (cat $ tail a)

getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ _) = func

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

generate :: Int -> [Agent]
generate num = take num $ zipWith3 (\func name (x,y) -> ((Agent func ((['a'..'z']!!(x-1)):'_':name) (x,y))))
                                                         (cycle agents) (cycle names) (cat [1..(ceiling $ sqrt $ fromIntegral num)])
    where agents = [pavlov,titForTat,sucker,grim,defector,mistrusting]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
