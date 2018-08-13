{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           CSV          (quoteListParser)
import qualified Data.Text.IO (getContents, getLine)

--
-- MAIN demostrate parsing a CSV file
--
main :: IO ()
main = do

  _ <- Data.Text.IO.getLine           -- skip header line
  csvData <- Data.Text.IO.getContents -- process remainder of file
  print $ quoteListParser csvData
