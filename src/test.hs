import Data.Tree
import Data.Tree.Zipper

t = Node "1" [Node "2" [Node "2" [],Node "3" [Node "4" [] ]], Node "5" [Node "6" [Node "7" []]]]

ft = show $ length $ forest $ findChild ((==) . (label 2)) $  fromTree t
main = putStrLn ft