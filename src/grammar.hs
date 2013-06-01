{-# LANGUAGE DeriveDataTypeable #-}

module Grammar
(
Expr(..),
Signature(..),
Kind(..),
arity,
signature
)
where

import qualified Data.Map as Map
import Data.Typeable
import Data.Generics
import Data.Char
import Data.List


data Kind = Float | Int | Bool | Any TVar | List Kind | Lambda Kind Kind | Val deriving(Eq,Show)



data Typeclass = Ord TVar | Numeric TVar | Eq TVar deriving (Show,Read,Eq)

data Expr = Head | Tail | If | Take | Null | Nth | Map | Filter | Fold | Compose | Equal | Not | Plus | Minus | Times
               | Mod | Divide | Round | Floor | Ceiling | ETrue | EFalse | Yours | Theirs | Rand | EInt Int | EFloat Float   {--Let--} deriving(Ord,Show,Eq)

data TVar = A | B | C | D | E | F | G | H | I | J | K | L
          | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving(Eq,Show,Ord,Enum,Read)

data Signature = Signature {classes:: [Typeclass], kinds :: [Kind]} deriving(Eq)
instance Show Signature where
  show s = " :: " ++ "(" ++ cls ++ ")" ++ " => " ++ kind
    where cls = foldr (\c str -> str++", "++(show c)) "" (classes s)
          kind = unwords $ intersperse "->" $ map show $ kinds s

signature :: Expr -> Maybe Signature
signature (EInt _) = Just $ Signature [] [Int]
signature (EFloat _ ) = Just $ Signature [] [Float]
signature expr = Map.lookup expr m
  where m = Map.fromList [ ( Head    , Signature [] [List (Any A), Any A])
                          , ( Tail    , Signature [] [List (Any A),Any A])
                          , ( If      , Signature [] [Bool,Any A,Any A,Any A])
                          , ( Take    , Signature [] [List (Any A), Int]    )
                          , ( Null    , Signature [] [List (Any A), Bool])
                          , ( Nth     , Signature [Numeric B] [Any B, List (Any A), Any B,Any A])
                          , ( Map     , Signature [] [(curry' [Any A, Any B]), List (Any A), List (Any B)])
                          , ( Filter  , Signature [] [(curry' [Any A, Bool]), List (Any A), List (Any A)])
                          , ( Fold    , Signature [] [(curry' [Any A,Any B,Any B]), List (Any A), Any B])
                          , ( Compose , Signature [] [(curry' [Any A,Any B]),(curry' [Any B,Any C]),(curry' [Any A,Any C])])
                          , ( Equal   , Signature [Eq A] [Any A,Any A, Bool])
                          , ( Not     , Signature [] [Bool,Bool])
                          , ( Plus    , Signature [Numeric A] arithmetic)
                          , ( Minus   , Signature [Numeric A] arithmetic)
                          , ( Times   , Signature [Numeric A] arithmetic)
                          , ( Mod     , Signature [Numeric A] arithmetic)
                          , ( Divide  , Signature [Numeric A] [Any A,Any A,Any A])
                          , ( Round   , Signature [Numeric A] [Any A,Int])
                          , ( Floor   , Signature [Numeric A] [Any A, Int])
                          , ( Ceiling , Signature [Numeric A] [Any A, Int])
                          , ( ETrue   , Signature [] [Bool])
                          , ( EFalse  , Signature [] [Bool])
                          , ( Yours   , Signature [] [List Bool])
                          , ( Theirs  , Signature [] [List Bool])
                          , ( Rand    , Signature [Numeric A] [Any A,Any A,Any A])
                          --, ( Let     , Signature [] [Val,Any A,Any A])
                         ]
        arithmetic = [ Any A, Any A, Any A]


arity :: Expr -> Maybe Int
arity expr = fmap ((subtract 1) . length . kinds) $  signature expr

curry' :: [Kind] -> Kind
curry' (a:b:[]) = Lambda a b
curry' (x:xs) = Lambda x (curry' xs)

uncurry' :: Kind -> [Kind]
uncurry' (Lambda val lambda) = val:uncurry' lambda
uncurry' x = []

