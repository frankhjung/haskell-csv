{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : CSV
  Description : Parse a CSV file.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  Example project to parse a CSV file using the
  [attoparsec](http://hackage.haskell.org/package/attoparsec) package.

-}

module CSV  (
              -- * Types
              Quote(..)
              -- * Parser Functions
            , csvFile
            , quote
            , quoteParser
              -- * Unit Test Data
            , testString
            , testQuote
            ) where

import           Data.Attoparsec.ByteString.Char8 (isAlpha_ascii)
import           Data.Attoparsec.Combinator       (endOfInput, many1)
import           Data.Attoparsec.Text             (Parser, char, double,
                                                   endOfLine, parseOnly,
                                                   takeTill)
import qualified Data.Attoparsec.Text             as DAT (takeWhile)
import           Data.Functor                     (($>))
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text, unpack)
import           Data.Time                        (LocalTime (..),
                                                   TimeOfDay (..),
                                                   defaultTimeLocale,
                                                   fromGregorian,
                                                   iso8601DateFormat,
                                                   parseTimeM)

-- | Stock Quotes
data Quote = Quote  {
                      qDate   :: LocalTime,
                      qStock  :: Text,
                      qShares :: Double,
                      qBasis  :: Double,
                      qPrice  :: Double
                    } deriving (Show, Eq)

-- | Read stock quotes from CSV file.
csvFile :: Parser [Quote]
csvFile = many1 quote <* endOfInput

-- | Stock quotes parser.
quote :: Parser Quote
quote = Quote <$> (qdate  <* qsep)
              <*> (qcode  <* qsep)
              <*> (double <* qsep)
              <*> (double <* qsep)
              <*> (double <* endOfLine)

-- | CSV field separator.
-- The default used here is the comma (U+2C).
qsep :: Parser ()
qsep = char ',' Data.Functor.$> ()

-- | Parse stock code.
qcode :: Parser Text
qcode = DAT.takeWhile isAlpha_ascii

-- | Parse ISO Date (YYYY-MM-DD).
qdate  :: Parser LocalTime
qdate  = createDate <$> takeTill (== ',')
         where defaultDate = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00 )
               parseDateText t = parseTimeM True Data.Time.defaultTimeLocale (iso8601DateFormat Nothing) (unpack t)
               createDate x = fromMaybe defaultDate $ parseDateText x

-- | Parser for stock quotes.
quoteParser :: Text -> Either String Quote
quoteParser = parseOnly quote

-- | [Unit testing](https://hspec.github.io/) Text input string.
testString :: Text
testString = "2018-08-05,ASX,100,23.4,25.6\n"

-- | [Unit testing](https://hspec.github.io/) Quote.
testQuote :: Quote
testQuote = Quote {
                    qDate   = LocalTime (fromGregorian 2018 08 05) (TimeOfDay 00 00 00),
                    qStock  = "ASX",
                    qShares = 100.0,
                    qBasis  = 23.4,
                    qPrice  = 25.6
                  }

