module Helpers
(cat,
 getFunction,
 reverseTuples,
 generate,
 neighbour,
 slice,
 getEmpty,
 getGrid,
 positions,
 prefix
) where

import Agent
import Data.List

merge :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

cat :: (Eq a) => [a] -> [(a,a)]
cat xs = [(x1,x2) | x1 <- xs, x2 <- xs]

getFunction :: Agent -> ([(Bool,Bool)] -> Bool)
getFunction (Agent func _ _) = func

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

neighbour :: Agent -> Agent -> Bool
neighbour (Agent _ _ (x,y)) (Agent _ _ (a,b) ) = (abs $ x-a) <= 1 || (abs $ y-b) <=1  --curently gets corners


generate :: Int -> [Agent] --off center
generate num = take num $ zipWith3 (\func name (x,y) -> ((Agent func ((['a'..'z']!!(abs $ x-1)):'_':name) (x,y))))
    (cycle agents) (cycle names) (cat range)
    where limit = round $ sqrt $ fromIntegral (num-2)
          agents = [pavlov,titForTat,sucker,grim,defector,mistrusting]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
          range = [(-limit)..(limit)]

getEmpty :: (Integral a) => [(a,a)] -> a -> [(a,a)]
getEmpty grid size = cat range \\ grid
    where limit = round $ sqrt $ fromIntegral size
          range = [-limit..limit]

getGrid :: [Agent] -> (Int,Int)
getGrid xs = foldr (\(a,b) (x,y) -> ((max a x),(max b y))) (head $ positions xs) (positions xs)

positions :: [Agent] -> [(Int,Int)]
positions x = map (\a -> position a) x

prefix :: (Show a) => [Char] -> a -> IO()
prefix pre string = putStrLn (pre++": "++(show $ string))





