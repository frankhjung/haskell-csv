module Main(main) where

import qualified Data.Text.IO (getContents, getLine)
import qualified QuotesCSV    (quoteListParser)

--
-- Demostrate parsing a CSV file.
-- It should return a list of Quotes.
--
main :: IO ()
main = do
  _ <- Data.Text.IO.getLine           -- skip header line
  csvData <- Data.Text.IO.getContents -- process remainder of file
  either (error "no quotes") (mapM_ print) (QuotesCSV.quoteListParser csvData)
