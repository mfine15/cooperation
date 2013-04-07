module Helpers
(cat,
 reverseTuples,
 neighbour,
 slice,
 getEmpty,
 getGrid,
 positions,
 output,
 findName,
 sameNames,
 unsafeRandom,
 permute,
 max2,
 maxTuple,
 powerset
) where

import Agent
import System.Random
import System.IO.Unsafe
import Types
import Data.List
import Genetics
import Control.Monad
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
neighbour a1 a2 = ((abs $ x-a) <= 1 || (abs $ y-b) <=1) && ((x/=a) && (y/=b))  --curently gets corners
    where (a,b) = position a1
          (x,y) = position a2

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
permute _ []   = []
permute gen xs = front : permute newGen (take n xs ++ drop (n+1) xs)
    where (n,newGen) = randomR (0,length xs -1) gen
          front = xs !! n

unsafeRandom :: (Random a) => (a,a) -> a
unsafeRandom (low,high) = unsafePerformIO $ getStdRandom $ randomR (low,high)

max2 :: DNA -> (String,String)
max2 xs = (title $ fst m1,title $ fst m2)
  where m1 = foldr1 (maxTuple) xs
        xs' = xs \\ [m1]
        m2 = foldr1 (maxTuple) xs'

maxTuple :: (Ord a) => (b,a) -> (b,a) -> (b,a)
maxTuple g1 g2 = if (snd g1) > (snd g2)
                then g1
                else g2
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])


