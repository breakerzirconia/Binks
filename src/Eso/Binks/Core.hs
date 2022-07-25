-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Eso.Binks.Core where

import           Data.Scientific
import           Data.Text       (Text, unpack)

data BinksValue
  = BinksNumber Scientific
  | BinksUnit
  deriving (Eq, Ord)

instance Show BinksValue where
  show (BinksNumber i) = show i
  show BinksUnit       = show ()

data Link a
  = Node a
  | Link a :><: Link a
  deriving (Eq, Ord)

instance Show a => Show (Link a) where
  showsPrec p = \case
    Node b   -> showString "[<" . shows b . showString ">]"
    a :><: b -> showParen True $ showsPrec 11 a . showString " " . showsPrec 11 b

instance {-# OVERLAPPING #-} Show (Link (Either Text BinksValue)) where
  showsPrec p = \case
    Node b   -> case b of
      Left txt -> showString $ unpack txt
      Right bv -> showString "[<" . shows bv . showString ">]"
    a :><: b -> showParen True $ showsPrec 11 a . showString " " . showsPrec 11 b

data Direction = L | R deriving (Eq, Ord)

instance Show Direction where
  show L = "<"
  show R = ">"

data Go = Once Direction | Forever Direction deriving (Eq, Ord)

instance Show Go where
  show (Once d)    = show d
  show (Forever d) = show d ++ "*"

data Instruction a
  = CreateLink Text (Link a)
  | Mirror Text Text
  | Access Text [Go]
  | Update Text [Go] (Either Text BinksValue)
  | Out Text
  | End
  deriving Eq

instance Show a => Show (Instruction a) where
  show = \case
    CreateLink txt li -> unwords [unpack txt, show li]
    Mirror txt e      -> unwords [unpack txt, "~" ++ unpack e]
    Access txt gos    -> unwords ["?", unpack txt, concatMap show gos]
    Update txt gos bv -> unwords ["!", unpack txt, concatMap show gos, show bv]
    Out txt           -> unwords ["out", unpack txt]
    End               -> "."

instance {-# OVERLAPPING #-} Show (Instruction (Either Text BinksValue)) where
  show = \case
    CreateLink txt li -> unwords [unpack txt, show li]
    Mirror txt e      -> unwords [unpack txt, "~" ++ unpack e]
    Access txt gos    -> unwords ["?", unpack txt, concatMap show gos]
    Update txt gos bv -> unwords ["!", unpack txt, concatMap show gos, either unpack show bv]
    Out txt           -> unwords ["out", unpack txt]
    End               -> "."
