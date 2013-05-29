{-# LANGUAGE DeriveDataTypeable #-}

module Grammar
(
Expr,
Signature,
Kind(..),
arity,
signatures
)
where

import qualified Data.Map as Map
import Data.Typeable
import Data.Generics
import Data.Char


data Kind = Float | Int | Bool | Any TVar | List Kind | Lambda Kind Kind | Class TVar

data Typeclass = Ord TVar | Numeric TVar | Eq TVar

data Expr = Head | Tail | If | Take | Null | Nth | Map | Filter | Fold | Compose | Equal | Not | Plus | Minus | Times
               | Mod | Divide | Round | Floor | Ceiling | ETrue | EFalse | Yours | Theirs | Rand | Number deriving(Ord,Show,Eq)

data TVar = A | B | C | D | E | F | G | H | I | J | K | L
          | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving(Eq,Show,Ord,Enum)

data Signature = Signature {classes:: [Typeclass], kinds :: [Kind]}

signatures :: Map.Map Expr Signature
signatures = Map.fromList [ ( Head    , Signature [] [List (Any A), Any A])
                          , ( Tail    , Signature [] [List (Any A),Any A])
                          , ( If      , Signature [] [Bool,Any A,Any A,Any A])
                          , ( Take    , Signature [] [List (Any A), Int]    )
                          , ( Null    , Signature [] [List (Any A), Bool])
                          , ( Nth     , Signature [Numeric B] [Class B, List (Any A), Any B,Any A])
                          , ( Map     , Signature [] [(makeCurry [Any A, Any B]), List (Any A), List (Any B)])
                          , ( Filter  , Signature [] [(makeCurry [Any A, Bool]), List (Any A), List (Any A)])
                          , ( Fold    , Signature [] [(makeCurry [Any A,Any B,Any B]), List (Any A), Any B])
                          , ( Compose , Signature [] [(makeCurry [Any A,Any B]),(makeCurry [Any B,Any C]),(makeCurry [Any A,Any C])])
                          , ( Equal   , Signature [Eq A] [Any A,Any A, Bool])
                          , ( Not     , Signature [] [Bool,Bool])
                          , ( Plus    , Signature [Numeric A] arithmetic)
                          , ( Minus   , Signature [Numeric A] arithmetic)
                          , ( Times   , Signature [Numeric A] arithmetic)
                          , ( Mod     , Signature [Numeric A] arithmetic)
                          , ( Divide  , Signature [Numeric A] [Class A,Class A,Class A])
                          , ( Round   , Signature [Numeric A] [Class A,Int])
                          , ( Floor   , Signature [Numeric A] [Class A, Int])
                          , ( Ceiling , Signature [Numeric A] [Class A, Int])
                          , ( ETrue   , Signature [] [Bool])
                          , ( EFalse  , Signature [] [Bool])
                          , ( Number  , Signature [Numeric A] [Class A, Class A])
                          , ( Yours   , Signature [] [List Bool])
                          , ( Theirs  , Signature [] [List Bool])
                          , ( Rand    , Signature [Numeric A] [Class A,Class A,Class A])
                         ]
  where arithmetic = [ Class A, Class A, Class A]


arity :: Expr -> Maybe Int
arity expr = fmap ((subtract 1) . length . kinds) $ Map.lookup expr signatures

makeCurry :: [Kind] -> Kind
makeCurry (a:b:[]) = Lambda a b
makeCurry (x:xs) = Lambda x (makeCurry xs)