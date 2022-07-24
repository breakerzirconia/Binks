-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

{-# LANGUAGE LambdaCase #-}

module Eso.Binks.Core where

import           Data.Set as Set

data Link = Node | Link :><: Link deriving (Eq, Ord)

instance Show Link where
  showsPrec p = \case
    Node :><: Node -> showString "1"
    Node           -> showString "0"
    a :><: b       -> showParen (p > 10) $ showsPrec 11 a . showsPrec 11 b

zero :: Link
zero = Node

one :: Link
one = zero :><: zero

setsOfLinksUpToLayer :: Int -> [Set Link]
setsOfLinksUpToLayer n =
  if n < 0
  then error "The designated layer number must be non-negative"
  else go 0 []
  where
    go :: Int -> [Set Link] -> [Set Link]
    go 0 _ = go 1 [singleton zero]
    go i s
      | i <= n =
        let uns = unions s
            cp = (\ ~(a, b) -> a :><: b) `Set.map` cartesianProduct uns uns
        in go (i + 1) (cp `difference` uns : s)
      | otherwise = reverse s

layer :: Link -> Int
layer = \case
  Node     -> 0
  a :><: b -> max (layer a) (layer b) + 1

leftRotation :: Link -> Link
leftRotation (a :><: (b :><: c)) = (a :><: b) :><: c
leftRotation s                   = s

rightRotation :: Link -> Link
rightRotation ((a :><: b) :><: c) = a :><: (b :><: c)
rightRotation s                   = s
