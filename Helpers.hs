module Helpers
(cat,
 getFunction,
 reverseTuples,
 generate
) where

import Agent
import Data.List

merge :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

cat :: (Eq a) => [a] -> [(a,a)]
cat [] = []
cat a = nubBy (\(x,y) (a,b) -> a == x && b == y) $ perms ++ opp
    where perms = zipWith (\a b -> (a,b)) (repeat $ head a) a ++ (cat $ tail a)
        -- generates all permutations, not just combinations
          opp = map (\(x,y) -> (y,x)) perms

getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ _) = func

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

generate :: Int -> [Agent]
generate num = take num $ zipWith3 (\func name (x,y) -> ((Agent func ((['a'..'z']!!(abs $ x-1)):'_':name) (x,y))))
                                                         (cycle agents) (cycle names) (cat [(-limit `div` 2)..(limit `div` 2)])
    where limit = ceiling $ sqrt $ fromIntegral (num+1)
          agents = [pavlov,titForTat,sucker,grim,defector,mistrusting]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
