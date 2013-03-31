module Helpers
(cat,
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
import Genetics
import System.Random
import Data.List
import Control.Parallel (par, pseq)


merge :: (Eq a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

cat :: (Eq a) => [a] -> [(a,a)]
cat xs = [(x1,x2) | x1 <- xs, x2 <- xs]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

reverseTuples :: [(a,a)] -> [(a,a)]
reverseTuples xs = map (\(a,b) -> (b,a)) xs

neighbour :: Agent -> Agent -> Bool
neighbour a1 a2 = (abs $ x-a) <= 1 || (abs $ y-b) <=1  --curently gets corners
    where (a,b) = position a1
          (x,y) = position a2


generate :: Int -> [Agent] -> IO [Agent]
generate num seed  = do
    gen <- getStdGen
    take num $ zipWith3 makeOne (cycle names) (cat $ range gen) (cycle $ composables gen)
    where limit  = round $ ((sqrt $ fromIntegral num)-1)/2
          names  = ["pavlov","titForTat","sucker","grim","defector","mistrusting"]
          range gen = permute gen [(-limit)..(limit)]
          rands gen = randomRs (0.0,1.0) gen
          composables gen = zipWith (\gene weight  -> (gene,weight)) (someGenes gen) (rands gen)
          someGenes gen = take (fst $ randomR (1,length staticGenes) gen) staticGenes

makeOne :: String -> (Int,Int) -> [(Gene,Float)] -> Agent
makeOne name (x,y) dna =
    Agent{
            function   = compose dna,
            name       = n name (x,y),
            position   = (x,y),
            generation = 1,
            genes = dna
        }
    where
        n name (x,y) = name++"("++show x++","++show y++  ")"




getEmpty :: (Integral a) => [(a,a)] -> a -> [(a,a)]
getEmpty grid size = cat range \\ grid
    where range = [-size..size]

getGrid :: [Agent] -> (Int,Int)
getGrid xs = foldr1 (\(a,b) (x,y) -> ((max (abs a) (abs x)),(max (abs b) (abs y)))) (positions xs)

positions :: [Agent] -> [(Int,Int)]
positions x = map (\a -> position a) x

output :: (Show a) => [Char] -> a -> IO()
output pre string = putStrLn (pre++": "++(show $ string))

sameNames :: Agent -> [Agent] -> [Agent]
sameNames n agents = filter (findName n) agents

findName :: Agent -> Agent -> Bool
findName a1 a2 = (slice 0 3 (name a1)) == (slice 0 3 (name a2))

permute :: StdGen -> [a] -> [a]
permute gen []  = []
permute gen xs  = (head tl) : permute gen' (hd ++ tail tl)
   where (idx, gen') = randomR (0,length xs - 1) gen
         (hd,  tl)   = splitAt idx xs