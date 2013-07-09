{-# LANGUAGE DeriveDataTypeable #-}

module Grammar
(
Expr(..),
Signature(..),
Type(..),
arity,
signature
)
where

import qualified Data.Map as Map
import Data.Typeable
import Data.Generics
import Data.Char
import Data.List
import Data.Maybe

class Infer t where
  sub :: Subst -> t -> t
  satisfies :: t -> Type -> Bool
  tv :: t -> [TVar]


type Subst = Map.Map TVar Type




data Type = Float | Int | Bool | Any TVar | List Type | Lambda Type Type  deriving(Eq,Show)






data Typeclass = Ord TVar | Numeric TVar | Eq TVar deriving (Show,Read,Eq)


data Expr = Head | Tail | If | Take | Null | Nth | Map | Filter | Fold | Compose | Equal | Not | Plus | Minus | Times
               | Mod | Divide | Round | Floor | Ceiling | ETrue | EFalse | Yours | Theirs | Rand | EInt Int | EFloat Float   {--Let--} deriving(Ord,Show,Eq,Read)

type TVar = String

data Signature = Abstract {classes:: [Typeclass], types :: [Type]}
               | Concrete [Type] deriving(Eq)
instance Show Signature where
  show s = " :: " ++ "(" ++ cls ++ ")" ++ " => " ++ kind
    where cls = foldr (\c str -> str++", "++(show c)) "" (classes s)
          kind = unwords $ intersperse "->" $ map show $ types s

nullSubst = []

subset :: Type -> Typeclass -> Bool
subset t c = elem t $ fromMaybe [] $ Map.lookup (head $ words $ show c) m
  where m = Map.fromList [ ("Ord",[Float,Int,Bool]),
                           ("Numeric",[Float,Int]),
                           ("Eq",[Float,Int,Bool,List Float,List Int,List Bool])
                          ]

newTVar :: TVar -> TVar
newTVar var = init var ++ (succ $ last var)

signature :: Expr -> Signature
signature (EInt _) = Abstract [] [Int]
signature (EFloat _ ) = Abstract [] [Float]
signature expr = fromJust $ Map.lookup expr m
  where m = Map.fromList [ ( Head     , Abstract [] [List (Any A), Any A])
                          , ( Tail    , Abstract [] [List (Any A),Any A])
                          , ( If      , Abstract [] [Bool,Any A,Any A,Any A])
                          , ( Take    , Abstract [] [List (Any A), Int]    )
                          , ( Null    , Abstract [] [List (Any A), Bool])
                          , ( Nth     , Abstract [Numeric B] [Any B, List (Any A), Any B,Any A])
                          , ( Map     , Abstract [] [(curry' [Any A, Any B]), List (Any A), List (Any B)])
                          , ( Filter  , Abstract [] [(curry' [Any A, Bool]), List (Any A), List (Any A)])
                          , ( Fold    , Abstract [] [(curry' [Any A,Any B,Any B]), List (Any A), Any B])
                          , ( Compose , Abstract [] [(curry' [Any A,Any B]),(curry' [Any B,Any C]),(curry' [Any A,Any C])])
                          , ( Equal   , Abstract [Eq A] [Any A,Any A, Bool])
                          , ( Not     , Abstract [] [Bool,Bool])
                          , ( Plus    , Abstract [Numeric A] arithmetic)
                          , ( Minus   , Abstract [Numeric A] arithmetic)
                          , ( Times   , Abstract [Numeric A] arithmetic)
                          , ( Mod     , Abstract [Numeric A] arithmetic)
                          , ( Divide  , Abstract [Numeric A] [Any A,Any A,Any A])
                          , ( Round   , Abstract [Numeric A] [Any A,Int])
                          , ( Floor   , Abstract [Numeric A] [Any A, Int])
                          , ( Ceiling , Abstract [Numeric A] [Any A, Int])
                          , ( ETrue   , Abstract [] [Bool])
                          , ( EFalse  , Abstract [] [Bool])
                          , ( Yours   , Abstract [] [List Bool])
                          , ( Theirs  , Abstract [] [List Bool])
                          , ( Rand    , Abstract [Numeric A] [Any A,Any A,Any A])
                          --, ( Let     , Signature [] [Val,Any A,Any A])
                         ]
        arithmetic = [ Any A, Any A, Any A]



arity :: Expr -> Int
arity expr = ((subtract 1) . length . types) $ signature expr

curry' :: [Type] -> Type
curry' (a:b:[]) = Lambda a b
curry' (x:xs) = Lambda x (curry' xs)

uncurry' :: Type -> [Type]
uncurry' (Lambda val lambda) = val:uncurry' lambda
uncurry' x = []

