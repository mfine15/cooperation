module Agent
(titForTat,
 sucker,
 grim,
) where

titForTat :: (Bool a) => [a] -> a
titForTat history
    | last history = True
    | otherwise    = False

grim :: (Bool a) => [a] -> a
grim history
    | not $ last history = False
    | not (xs !! (length xs -1)) = False
    | otherwise = True

sucker :: (Bool a) => [a] -> a
sucker history = True
