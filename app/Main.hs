{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           CSV                (quoteListParser)
import qualified Data.Text.IO       (readFile)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (die)

--
-- MAIN demostrate parsing a CSV file
--
main :: IO ()
main = do

    args <- getArgs
    if length args == 1
      then do
        csvData <- Data.Text.IO.readFile (head args)
        print $ quoteListParser csvData
      else do
        progName <- getProgName
        die $ concat ["Usage: ", progName, " <csv file>\n", "Error: Require file name"]

