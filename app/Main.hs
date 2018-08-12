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
usage :: Maybe String -> IO ()
usage s = do
  progName <- System.Environment.getProgName
  let usageMessage = "Usage: " ++ progName ++ " <csv file>\n"
  case s of
    Nothing -> System.Exit.die usageMessage
    Just e  -> System.Exit.die (usageMessage ++ e)

--
-- MAIN demostrate parsing a CSV file
--
main :: IO ()
main = do

    args <- System.Environment.getArgs

    when (length args /= 1) $ usage (Just "Require file name")

    csvData <- Data.Text.IO.readFile (head args)

    -- returns an list of quotes, same as:
    print $ rights $ map quoteTextParser $ (tail . Data.Text.lines) csvData

