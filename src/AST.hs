module AST
where

import Grammar
import Data.Tree
import Data.Tree.Zipper
import Data.List
import qualified Data.Map as Map


type Constraint = (Type,Type)
data UTree = UTree {val :: Expr, children :: [UTree]} deriving (Eq,Show)
  instance Functor UTree where
    fmap f (UTree node subTree) = UTree (fmap node) (map (fmap f) subTree)

mapUp :: ([Signature] -> Expr -> (Expr,Signature)) -> Tree Expr ->  Tree (Expr,Signature)
mapUp f tree acc = if isNothing $ parent pos
                   then Node ((label pos),f acc (label pos))
                   else
  where pos = fromTree tree

bottom :: Tree Expr -> [TreePos Full Expr]
bottom tree = realBottom [] (fromTree tree)
  where realBottom list tree =
                if hasChildren treeloc
                then (label treeloc):list
                else foldr ((++) . (realBottom [])) [] (map fromTree subForest $ tree treeloc)
              where treeloc = fromTree tree
        realBottom :: [Expr] -> Tree Expr -> [TreePos Full Expr]}

secondBottom = map fromTree $ nub $ filter (not . null) $ map fromMaybe [] parent

bottom' :: Tree Expr -> [Expr]
bottom' tree = realBottom [] tree
  where realBottom list tree =
                if null $ subForest tree
                then (rootLabel tree):list
                else foldr ((++) . (realBottom [])) [] (children tree)
        realBottom :: [Expr] -> Tree Expr -> [Expr]

infer :: Tree Expr -> Maybe (Tree (Expr,Signature))
infer (Tree value []) = signature value
infer (Tree value children) = makeFit (signature value) children
  where makeFit :: Signature -> [Expr] -> Maybe Signature
        makeFit =