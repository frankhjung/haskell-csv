{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           Control.Monad      (when)
import           CSV                (quoteTextParser)
import           Data.Either        (rights)
import           Data.Text          (lines)
import           Data.Text.IO       (readFile)
import qualified System.Environment (getArgs, getProgName)
import qualified System.Exit        (die)

-- Usage with current program name and command arguments
usage :: IO ()
usage = do
  progName <- System.Environment.getProgName
  System.Exit.die $ concat ["Usage: ", progName, " <csv file>\nError: Require file name"]

--
-- MAIN demostrate parsing a CSV file
--
main :: IO ()
main = do

    args <- System.Environment.getArgs
    when (length args /= 1) usage

    csvData <- Data.Text.IO.readFile (head args)
    -- returns an list of quotes
    print $ rights $ map quoteTextParser $ (tail . Data.Text.lines) csvData

