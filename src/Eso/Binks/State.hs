-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.State where

import           Data.Map.Strict as Map
import           Data.Text

import           Eso.Binks.Core

data BinksState = BinksState
  { ids :: Map Text (Link BinksValue)
  }

start :: BinksState
start = BinksState Map.empty
