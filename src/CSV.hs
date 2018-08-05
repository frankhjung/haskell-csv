{-# LANGUAGE OverloadedStrings #-}

module CSV  (
              Quote(..)
            , csvFile
            , quote
            , quoteParser
            , testString
            , testQuote
            ) where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Maybe
import           Data.Text
import           Data.Time

data Quote = Quote  {
                      qDate   :: LocalTime,
                      qShares :: Double,
                      qBasis  :: Double,
                      qPrice  :: Double
                    } deriving (Show, Eq)

csvFile :: Parser [Quote]
csvFile = many1 quote <* endOfInput

quote :: Parser Quote
quote = Quote <$> (qdate  <* qsep)
              <*> (double <* qsep)
              <*> (double <* qsep)
              <*> (double <* endOfLine)

-- define the separator
qsep :: Parser ()
qsep = char ',' Data.Functor.$> ()

-- parse and ISO Date (YYYY-MM-DD)
qdate  :: Parser LocalTime
qdate  = createDate <$> takeTill (== ',')
         where defaultDate = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00 )
               parseDateText t = parseTimeM True Data.Time.defaultTimeLocale (iso8601DateFormat Nothing) (unpack t)
               createDate x = fromMaybe defaultDate $ parseDateText x

quoteParser :: Text -> Either String Quote
quoteParser = parseOnly quote

-- for unit testing
testString :: Text
testString = "2018-08-05,100,23.4,25.6\n"

-- for unit testing
testQuote :: Quote
testQuote = Quote {
                    qDate = LocalTime (fromGregorian 2018 08 05) (TimeOfDay 00 00 00),
                    qShares = 100.0,
                    qBasis = 23.4,
                    qPrice = 25.6
                  }

