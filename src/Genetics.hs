module Genetics
(
Gene(..),
compose,
staticGenes
) where


data Gene = Gene {
                  title :: String,
                  fn    :: ([(Bool,Bool)] -> Bool)
                 }
instance Show Gene where
    show gene = title gene
instance Eq Gene where
    (==) g1 g2 = title g1 == title g2

compose :: [(Gene,Float)] -> ([(Bool,Bool)] -> Bool)
compose genes = \xs -> (fn $ fst $ foldr1 amalgate genes) xs

amalgate :: (Gene,Float) -> (Gene,Float) -> (Gene,Float)
amalgate (g1,weight1) (g2,weight2) = (Gene "Composite" func,1)
    where
        -- Runs the history through the individual functions, then multiplies by the weight, and add them
        -- returning a new function, which returns true if greater than the average weight
        func xs = result1 + result2 > avg
            where avg = weight1 + weight2 / 2
                  result1 = (boolInt $ fn g1 $ xs) * weight1
                  result2 = (boolInt $ fn g2 $ xs) * weight2
        boolInt x = if x then 1 else 0

