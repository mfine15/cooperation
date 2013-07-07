module AST
where

import Grammar
import Data.Tree
import Data.Tree.Zipper
import Data.List
import qualified Data.Map as Map


type Annotation = (Expr,Signature)

type Constraint = (Type,Type)

type UTree = Tree Expr

type TTree = Tree (Expr,Signature)
treeScan :: (Expr -> Signature -> Annotation) -> UTree -> TTree
treeScan f tree = realBottom

scan :: (a -> b -> a) -> Tree b -> Tree a
scan f tree = if not null tree
              then
              else f
  where b = bottom tree

mapUp :: (Signature -> Expr -> Annotation) -> Tree Expr -> TTree
mapUp f tree =
  where pos = fromTree tree
        scan :: (Signature -> Expr -> Annotation) -> TreePos Full Expr -> [TTree] -> [TTree]
        scan f treepos acc = if isNothing $ parent $ parent treepos
                             then map scan (f (map (last . types . snd . rootLabel) acc) (label treepos))

                             else

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

infer :: Tree Expr -> Maybe (TTree)
infer (Tree value []) = signature value
infer (Tree value children) = map $ \tree ->

  where makeFit :: Signature -> [Expr] -> Maybe [(Expr,Type)]
        makeFit = []
        determine ::