{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import qualified Data.Text.IO (getContents, getLine)
import           Fmt          (commaizeF, fixedF, fmtLn)
import           Lib          (Quote (..), quoteListParser)

--
-- Demostrate parsing a CSV file.
-- It should return a list of Quotes.
--

-- | Get the average price for a list of quotes.
averagePrice :: [Quote] -> Double
averagePrice [] = 0.0
averagePrice qs = sum (map qPrice qs) / fromIntegral (length qs)

-- | Demostrate parsing a CSV file.
-- Parse CSV data and print average price.
main :: IO ()
main = do
  _ <- Data.Text.IO.getLine                           -- skip header line
  csvData <- Data.Text.IO.getContents                 -- process remainder of file
  let quotes = either (error . show) id (quoteListParser csvData)
  mapM_ print quotes
  fmtLn $ "Number of Quotes read: " <> commaizeF (length quotes)
  fmtLn $ "Average price: " <> fixedF 2 (averagePrice quotes)
