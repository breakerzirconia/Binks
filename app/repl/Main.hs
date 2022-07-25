-- Copyright (c) 2022 Constantine Ter-Matevosian
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Main where

import           Control.Monad.State
import           Data.Text
import           System.Console.Haskeline

import           Eso.Binks.Repl
import           Eso.Binks.State

main :: IO ()
main = runInputT defaultSettings (loop start)
  where
    loop :: BinksState -> InputT IO ()
    loop stt = do
      minput <- getInputLine "binksi> "
      case minput of
        Nothing    -> outputStrLn "Bye-bye!"
        Just "bye" -> outputStrLn "Bye-bye!"
        Just input -> process (pack input) stt >>= loop
