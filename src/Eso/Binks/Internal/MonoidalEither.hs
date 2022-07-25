-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.Internal.MonoidalEither where

import           Control.Applicative

data MEither e a
  = MLeft e
  | MRight a
  deriving (Eq, Show)

meither :: (e -> b) -> (a -> b) -> MEither e a -> b
meither f g = \case
  MLeft e  -> f e
  MRight a -> g a

fromMLeft :: Monoid e => MEither e a -> e
fromMLeft = \case
  MLeft e  -> e
  MRight a -> mempty

instance Functor (MEither e) where
  fmap f = \case
    MLeft e  -> MLeft e
    MRight a -> MRight $ f a

instance Monoid e => Applicative (MEither e) where
  pure = MRight

  MRight f <*> MRight a = MRight $ f a
  e <*> e'              = MLeft $ fromMLeft e `mappend` fromMLeft e'

instance Monoid e => Monad (MEither e) where
  me >>= f = case me of
    MLeft e  -> MLeft e
    MRight a -> f a

instance Monoid e => Alternative (MEither e) where
  empty = MLeft mempty

  MRight a <|> _ = MRight a
  e <|> e'       = MLeft $ fromMLeft e `mappend` fromMLeft e'
