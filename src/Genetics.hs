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


parse :: Expr -> Maybe (Tree String)
parse [] = Nothing
parse (x:xs) = Just (Node x (map fromParse (take (fromJust $ Map.lookup x arity) xs)))
  where
        fromParse x = if isJust p then fromJust p else []
          where p = parse x

getType :: Tree Expr -> Maybe Signature
getType tree =

secondBottom :: Tree a -> TreeLoc a
secondBottom tree = if
  where zipper = fromTree tree
        children = subForest tree
        isLast = foldr (\acc child -> acc && null child) True children