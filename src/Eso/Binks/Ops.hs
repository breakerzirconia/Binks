-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.Ops where

import qualified Data.Map.Strict                   as Map
import           Data.Set                          as Set
import           Data.Text                         as Text hiding (reverse)

import           Eso.Binks.Core
import           Eso.Binks.Internal.MonoidalEither
import           Eso.Binks.State

zero :: Link BinksValue
zero = Node BinksUnit

one :: Link BinksValue
one = zero :><: zero

setsOfLinksUpToLayer :: Int -> [Set (Link BinksValue)]
setsOfLinksUpToLayer n =
  if n < 0
  then error "The designated layer number must be non-negative"
  else go 0 []
  where
    go :: Int -> [Set (Link BinksValue)] -> [Set (Link BinksValue)]
    go 0 _ = go 1 [Set.singleton zero]
    go i s
      | i <= n =
        let uns = unions s
            cp = (\ ~(a, b) -> a :><: b) `Set.map` cartesianProduct uns uns
        in go (i + 1) (cp `difference` uns : s)
      | otherwise = reverse s

height :: Link a -> Int
height = \case
  Node _   -> 0
  a :><: b -> max (height a) (height b) + 1

mirror :: Link a -> Link a
mirror = \case
  l :><: l' -> mirror l' :><: mirror l
  l         -> l

leftRotation :: Link a -> Link a
leftRotation (a :><: (b :><: c)) = (a :><: b) :><: c
leftRotation s                   = s

rightRotation :: Link a -> Link a
rightRotation ((a :><: b) :><: c) = a :><: (b :><: c)
rightRotation s                   = s

substitute
  :: Link (Either Text BinksValue)
  -> BinksState
  -> MEither [Text] (Link BinksValue)
substitute li stt@BinksState{..} = case li of
  Node e       -> case e of
    Left txt -> case ids Map.!? txt of
      Nothing  -> MLeft [txt]
      Just li' -> MRight li'
    Right bv -> MRight $ Node bv
  l :><: r -> (:><:) <$> substitute l stt <*> substitute r stt

deep :: Link a                       -- ^ The original Link
     -> [Go]                         -- ^ The list of directions
     -> (Link a -> Link a -> Link a) -- ^ The link structure modifier when going to the left subtree
     -> (Link a -> Link a -> Link a) -- ^ The link structure modifier when going to the right subtree
     -> (Link a -> Link a)           -- ^ The post-processing function
     -> Link a                       -- ^ The resulting link
deep (l :><: r) gos@(d : ds) f g postprocess = case d of
  Once di -> case di of
    L -> deep l ds f g postprocess `f` r
    R -> l `g` deep r ds f g postprocess
  Forever di -> case di of
    L -> deep l gos f g postprocess `f` r
    R -> l `g` deep r gos f g postprocess
deep l _ _ _ postprocess = postprocess l
