{-# LANGUAGE LambdaCase #-}

module Eso.Lang where

import           Data.Set as Set

data Struct = Node | Struct :--: Struct deriving (Eq, Ord)

instance Show Struct where
  showsPrec p = \case
    Node :--: Node -> showString "1"
    Node           -> showString "0"
    a :--: b       -> showParen (p > 10) $ showsPrec 11 a . showsPrec 11 b

zero :: Struct
zero = Node

one :: Struct
one = zero :--: zero

setsOfStructsUpToLayer :: Int -> [Set Struct]
setsOfStructsUpToLayer n =
  if n < 0
  then error "The designated layer number must be non-negative"
  else go 0 []
  where
    go :: Int -> [Set Struct] -> [Set Struct]
    go 0 _ = go 1 [singleton zero]
    go i s
      | i <= n =
        let uns = unions s
            cp = (\ ~(a, b) -> a :--: b) `Set.map` cartesianProduct uns uns
        in go (i + 1) (cp `difference` uns : s)
      | otherwise = reverse s

layer :: Struct -> Int
layer = \case
  Node     -> 0
  a :--: b -> max (layer a) (layer b) + 1
