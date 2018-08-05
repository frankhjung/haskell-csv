{-# LANGUAGE OverloadedStrings #-}

module CSV (
              Quote(..)
            , csvFile
            , quote
            , quoteParser
            , testString
           ) where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Maybe
import           Data.Text
import           Data.Time

csvFile :: Parser [Quote]
csvFile = many1 quote <* endOfInput

data Quote = Quote {
              qDate   :: LocalTime,
              qShares :: Double,
              qBasis  :: Double,
              qPrice  :: Double
             } deriving (Show, Eq)

quote   :: Parser Quote
quote   = Quote <$> (qdate <* qcomma)
                <*> (double <* qcomma)
                <*> (double <* qcomma)
                <*> (double <* endOfLine)

qcomma  :: Parser ()
qcomma  = char ',' Data.Functor.$> ()

qdate   :: Parser LocalTime
qdate   = createDate <$> takeTill (== ',')
    where   defaultDate = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00 )
            parseDateText t = parseTimeM True Data.Time.defaultTimeLocale (iso8601DateFormat Nothing) (unpack t)
            createDate x = fromMaybe defaultDate $ parseDateText x

quoteParser :: Text -> Either String Quote
quoteParser = parseOnly quote

testString :: Text
testString = "2018-08-05,100,23.4,25.6\n"

