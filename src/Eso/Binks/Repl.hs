-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Eso.Binks.Repl where

import           Data.Map.Strict                   as Map
import           Data.Text
import           System.Console.Haskeline
import           Text.Megaparsec

import           Eso.Binks.Core
import           Eso.Binks.Internal.MonoidalEither
import           Eso.Binks.Internal.Parser
import           Eso.Binks.Ops
import           Eso.Binks.State

process :: Text -> BinksState -> InputT IO BinksState
process txt stt@BinksState{..} = case parseMaybe instruction txt of
  Nothing  -> outputStrLn "Unknown instruction" >> return stt
  Just ins -> do
    outputStrLn $ show ins
    case ins of
      CreateLink txt li -> case substitute li stt of
          MLeft txts -> do
            outputStrLn $ "Could not substitute the following links:\n" ++
                          unpack (Data.Text.unlines $ (\txt -> "- `" <> txt <> "`") <$> txts)
            return stt
          MRight li' -> return . BinksState $ Map.insert txt li' ids
      Mirror txt txt' -> case ids Map.!? txt' of
        Nothing -> do
          outputStrLn $ "The link `" ++ unpack txt' ++ "` does not exist"
          return stt
        Just li -> return . BinksState $ Map.insert txt (mirror li) ids
      Access txt gos -> do
        outputStrLn $ case ids Map.!? txt of
          Nothing -> "The link `" ++ unpack txt ++ "` does not exist"
          Just li -> show (deep li gos const (\_ x -> x) id)
        return stt
      Update txt gos e  -> case ids Map.!? txt of
        Nothing -> do
          outputStrLn $ "The link `" ++ unpack txt ++ "` does not exist"
          return stt
        Just li -> case e of
          Left txt' -> case ids Map.!? txt' of
            Nothing -> do
              outputStrLn $ "The link `" ++ unpack txt ++ "` does not exist"
              return stt
            Just li' -> return . BinksState $
              Map.insert txt (deep li gos (:><:) (:><:) (const li')) ids
          Right bv  -> return . BinksState $
            Map.insert txt (deep li gos (:><:) (:><:) (const $ Node bv)) ids
      Out txt -> do
        outputStrLn $ case ids Map.!? txt of
          Nothing -> "The link `" ++ unpack txt ++ "` does not exist"
          Just li -> show li
        return stt
      End -> return stt
