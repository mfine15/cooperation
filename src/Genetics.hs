module Genetics
(
Gene(..),
DNA,
compose,
staticGenes,
breed
) where

import System.IO.Unsafe
import System.Random
import Data.List
import Data.Ord
import Data.Function
import Types


breed :: DNA -> DNA -> DNA
breed dna1 dna2 = zipWith (\(gene,_) weight -> (gene,weight)) sorted sortRand
  where gen = unsafeRandom (1,100)
        rands = randomRs (0.0,1.0) (mkStdGen gen)
        combined = dna1 ++ dna2
        sorted   = sortBy (compare `on` snd) combined
        sortRand = sort $ take (unsafeRandom (1,length combined) :: Int) rands

compose :: DNA -> ([(Bool,Bool)] -> Bool)
compose genes = \xs -> (fn $ fst $ foldr1 amalgate genes) xs

amalgate :: (Gene,Float) -> (Gene,Float) -> (Gene,Float)
amalgate (g1,weight1) (g2,weight2) = (Gene "Composite" func,1)
    where
        -- Runs the history through the individual functions, then multiplies by the weight, and add them
        -- returning a new function, which returns true if greater than the average weight
        func xs = result1 + result2 > avg
            where avg = weight1 + weight2 / 2
                  result1 = boolInt (fn g1 $ xs) * weight1
                  result2 = boolInt (fn g2 $ xs) * weight2
        boolInt x = if x then 1 else 0

staticGenes = [ Gene "indignant" indignant,
                Gene "mean" mean,
                Gene "nice" nice,
                Gene "repeptive" repeptive,
                Gene "grudge" grudge,
                Gene "locus" locus,
                Gene "arbitrary" arbitrary
              ]
indignant,mean,nice,repeptive,grudge,locus :: [(Bool,Bool)] -> Bool
indignant history  = null history ||
                    ((realToFrac $ length trues)/(realToFrac $ length history) > 0.5)
  where trues = filter fst history

mean history  = False
nice history  = True
repeptive history  = null history ||
                     (snd $ head history)
grudge history  = null history ||
                  (fst $ head history) && (snd $ head history)
-- If a person has an increased locus of control, they will grow increasingly
-- agitated if they start loosing to the other person
locus history  = if null history then True
                  else fst $ head history
arbitrary _ = even $ (unsafeRandom (1,100) :: Int)


unsafeRandom :: (Random a) => (a,a) -> a
unsafeRandom (low,high) = unsafePerformIO $ getStdRandom $ randomR (low,high)