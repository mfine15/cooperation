module Genetics
(
Gene(..),
compose,
staticGenes
) where


data Gene = Gene {
                  title :: String,
                  fn    :: ([(Bool,Bool)] -> Int -> Bool)
                 }
instance Show Gene where
    show gene = title gene
instance Eq Gene where
    (==) g1 g2 = title g1 == title g2

compose :: [(Gene,Float)] -> ([(Bool,Bool)] -> Int -> Bool)
compose genes = \xs num -> (fn $ fst $ foldr1 amalgate genes) xs

amalgate :: (Gene,Float) -> (Gene,Float) -> (Gene,Float)
amalgate (g1,weight1) (g2,weight2) = (Gene "Composite" func,1)
    where
        -- Runs the history through the individual functions, then multiplies by the weight, and add them
        -- returning a new function, which returns true if greater than the average weight
        func xs num = result1 + result2 > avg
            where avg = weight1 + weight2 / 2
                  result1 = (boolInt (fn g1 $ xs num) * weight1
                  result2 = (boolInt (fn g2 $ xs num)) * weight2
        boolInt x = if x then 1 else 0

staticGenes = [ Gene "indignant" indignant,
                Gene "mean" mean,
                Gene "nice" nice,
                Gene "repeptive" repeptive,
                Gene "grudge" grudge,
                Gene "locus" locus,
                Gene "random" random
              ]
indignant,mean,nice,repeptive,grudge,locus,random :: [(Bool,Bool)] -> Int -> Bool

indignant history _ = if null history then True
                    else ((realToFrac $ length trues)/(realToFrac $ length history) > 0.5)
  where trues = filter fst history

mean history _ = False

nice history _ = True

repeptive history _ = if null history then True
                    else snd $ head history

grudge history _ = if null history then True
                  else (fst $ head history) && (snd $ head history)

-- If a person has an increased locus of control, they will grow increasingly
-- agitated if they start loosing to the other person
locus history _ = if null history then True
                  else fst $ head history
random _ num = even num


