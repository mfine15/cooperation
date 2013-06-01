module AST
where

import Grammar
import Data.Tree
import Data.Tree.Zipper
import Data.List


data Zipper = Zipper {parents :: [Tree], lefts :: [Zipper], rights :: [Zipper], children :: [Zipper] , node :: Expr, treeChildren :: [Expr]}

data UTree = UTree {val :: Expr, children :: [UTree]} deriving (Eq,Show)
  instance Functor UTree where
    fmap f (UTree node subTree) = UTree (fmap node) (map (fmap f) subTree)



bottom :: Tree Expr -> [TreePos Full Expr]
bottom tree = realBottom [] (fromTree tree)
  where realBottom list tree =
                if hasChildren treeloc
                then (label treeloc):list
                else foldr ((++) . (realBottom [])) [] (map fromTree subForest $ tree treeloc)
              where treeloc = fromTree tree
        realBottom :: [Expr] -> Tree Expr -> [TreePos Full Expr]}