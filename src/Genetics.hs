module Genetics
(
Gene(..),
compose,
staticGenes
) where

import Data.Tree
import qualified Data.Map as Map
import Data.Maybe
import Grammar
import Language.Haskell.TH
data Gene = Gene {
                  standard :: Bool,
                  tree :: Tree Expr
                 }
instance Show Gene where
    show gene = show $ khead gene ++ show $ ktail gene
instance Eq Gene where
    (==) g1 g2 = show g1 == show g2 && standard g1 == standard g2


