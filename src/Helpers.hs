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
 output,
 findName,
 sameNames
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
generate num = take num $ zipWith3 (\func name (x,y) -> ((Agent func (name++"("++show x++",)") (x,y))))
    (cycle agents) (cycle names) (cat range)
    where limit = floor $ sqrt $ fromIntegral (num-2)
          agents = [pavlov,titForTat,sucker,grim,defector,mistrusting]
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
          range = [(-limit)..(limit)]

getEmpty :: (Integral a) => [(a,a)] -> a -> [(a,a)]
getEmpty grid size = cat range \\ grid
    where limit = round $ sqrt $ fromIntegral size
          range = [-limit..limit]

getGrid :: [Agent] -> (Int,Int)
getGrid xs = foldr1 (\(a,b) (x,y) -> ((max a x),(max b y))) (positions xs)

positions :: [Agent] -> [(Int,Int)]
positions x = map (\a -> position a) x

output :: (Show a) => [Char] -> a -> IO()
output pre string = putStrLn (pre++": "++(show $ string))

sameNames :: Agent -> [Agent] -> [Agent]
sameNames n agents = filter (findName n) agents

findName :: Agent -> Agent -> Bool
findName (Agent _ n1 _ ) (Agent _ n2 _) = (slice 0 3 n1) == (slice 0 3 n2)





