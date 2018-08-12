{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           Control.Monad      (when)
import           CSV                (quoteTextParser)
import           Data.Either        (rights)
import           Data.Text          (lines)
import           Data.Text.IO       (readFile)
import           Prelude            hiding (lines, readFile)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (die)

-- Usage with current program name and command arguments
usage :: IO ()
usage = do
  progName <- getProgName
  die $ concat ["Usage: ", progName, " <csv file>\nError: Require file name"]

--
-- MAIN demostrate parsing a CSV file
-- TODO simplify by
-- (1) providing a quoteFileParser function FilePath -> [Quotes]
-- (2) letting quoteTextParser work with [Text] -> [Either String Quote]
--
main :: IO ()
main = do

    args <- getArgs
    when (length args /= 1) usage

    csvData <- readFile (head args)
    -- returns an list of quotes
    print $ rights $ map quoteTextParser $ (tail . lines) csvData

