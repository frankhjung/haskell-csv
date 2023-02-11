{-|
  Module      : QuotesCSV
  Description : Parse a CSV file containing stock quotes.
  Copyright   : © Frank Jung, 2018-2023
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable

  Example project to parse a CSV file using the
  [attoparsec](http://hackage.haskell.org/package/attoparsec) package.

-}

module QuotesCSV  (
              -- * Types
              Quote(..)
              -- * Parser Functions
            , quote
            , quoteTextParser
            , quotes
            , quoteListParser
            ) where

import           Data.Attoparsec.ByteString.Char8 (isAlpha_ascii)
import           Data.Attoparsec.Text             (Parser, char, double,
                                                   endOfInput, endOfLine, many1,
                                                   parseOnly, takeTill)
import qualified Data.Attoparsec.Text             (takeWhile)
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
                    } deriving (Eq, Show)

-- | Stock quotes parser.
quote :: Parser Quote
quote = Quote <$> (qdate  <* qsep)
              <*> (qcode  <* qsep)
              <*> (double <* qsep)
              <*> (double <* qsep)
              <*> (double <* endOfLine)

-- | Parser wrapper for a single stock quote.
quoteTextParser :: Text -> Either String Quote
quoteTextParser = parseOnly quote

-- | Parser for a list of stock quotes.
quotes :: Parser [Quote]
quotes = do
  q <- many1 quote
  endOfInput
  return q

-- | Parser wrapper for a list of stock quotes.
quoteListParser :: Text -> Either String [Quote]
quoteListParser = parseOnly quotes

-- | CSV field separator.
-- The default used here is the comma, (U+2C).
qsep :: Parser ()
qsep = char ',' Data.Functor.$> ()

-- | Parse stock code.
qcode :: Parser Text
qcode = Data.Attoparsec.Text.takeWhile isAlpha_ascii

-- | Parse ISO Date (YYYY-MM-DD).
qdate  :: Parser LocalTime
qdate  = createDate <$> takeTill (== ',')
         where defaultDate = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00 )
               parseDateText t = parseTimeM True Data.Time.defaultTimeLocale (iso8601DateFormat Nothing) (unpack t)
               createDate x = fromMaybe defaultDate $ parseDateText x
