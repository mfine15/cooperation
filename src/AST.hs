module AST
where

import Data.Tree
import Grammar
import Data.Tree.Zipper
import Data.List

{--parse :: Expr -> Maybe (Tree String)
parse [] = Nothing
parse (x:xs) = Just (Node x (map fromParse (take (fromJust $ Map.lookup x arity) xs)))
  where
        fromParse x = if isJust p then fromJust p else []
          where p = parse x --}



secondBottom :: Tree a -> [TreePos Full a]
secondBottom tree = if isLast
                    then before children ++ after children
                    else nub $ concatMap secondBottom children
  where zipper = fromTree tree
        children = subForest tree
        isLast = foldr (\child acc -> acc && null child) True children