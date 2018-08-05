{-# LANGUAGE OverloadedStrings #-}

module CSV (
              Quote(..)
            , csvFile
            , quote
           ) where

import           Control.Applicative
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Maybe
import           Data.Text
import           Data.Time
import           System.Locale

csvFile :: Parser [Quote]
csvFile = many1 quote <* endOfInput

data Quote = Quote {
              qDate   :: LocalTime,
              qCode   :: String,
              qShares :: Double,
              qBasis  :: Double,
              qPrice  :: Double
             } deriving (Show, Eq)

quote   :: Parser Quote
quote   = Quote <$> (qdate <* qcomma)
                <*> (string <* qcomma)
                <*> (double <* qcomma)
                <*> (double <* qcomma)
                <*> (double <* endOfLine)

qcomma  :: Parser ()
qcomma  = char ',' Data.Functor.$> ()

qdate   :: Parser LocalTime
qdate   = createDate <$> takeTill (== ',')
    where   defaultDate = Data.Time.iso8601DateFormat
            parseTimeText d = parseTime Data.Time.defaultTimeLocale "%Y-%m-%d" (unpack d)
            createDate x = fromMaybe defaultDate $ parseTimeText x

quoteParser = parseOnly quote

