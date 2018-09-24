module Main(main) where

import qualified CSV          (quoteListParser)
import qualified Data.Text.IO (getContents, getLine)

--
-- MAIN demostrate parsing a CSV file.
-- It should return a list of Quotes.
--
main :: IO ()
main = do

  _ <- Data.Text.IO.getLine           -- skip header line
  csvData <- Data.Text.IO.getContents -- process remainder of file
  either (error "no quotes") (mapM_ print) (CSV.quoteListParser csvData)
