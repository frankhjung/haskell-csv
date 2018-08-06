{-# LANGUAGE OverloadedStrings #-}

module CSV  (
              Quote(..)
            , csvFile
            , quote
            , quoteParser
            , testString
            , testQuote
            ) where

import           Data.Attoparsec.ByteString.Char8 (isAlpha_ascii)
import           Data.Attoparsec.Combinator       (endOfInput, many1)
import           Data.Attoparsec.Text             (Parser, char, double,
                                                   endOfLine, parseOnly,
                                                   takeTill)
import qualified Data.Attoparsec.Text             as DAT (takeWhile)
import qualified Data.Functor                     as DF (($>))
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text, unpack)
import           Data.Time                        (LocalTime (..),
                                                   TimeOfDay (..),
                                                   defaultTimeLocale,
                                                   fromGregorian,
                                                   iso8601DateFormat,
                                                   parseTimeM)

-- stock quotes
data Quote = Quote  {
                      qDate   :: LocalTime,
                      qStock  :: Text,
                      qShares :: Double,
                      qBasis  :: Double,
                      qPrice  :: Double
                    } deriving (Show, Eq)

-- read stock quotes from CSV file
csvFile :: Parser [Quote]
csvFile = many1 quote <* endOfInput

-- parser stock quotes
quote :: Parser Quote
quote = Quote <$> (qdate  <* qsep)
              <*> (qcode  <* qsep)
              <*> (double <* qsep)
              <*> (double <* qsep)
              <*> (double <* endOfLine)

-- CSV field separator
qsep :: Parser ()
qsep = char ',' DF.$> ()

-- parse code
qcode :: Parser Text
qcode = DAT.takeWhile isAlpha_ascii

-- parse ISO Date (YYYY-MM-DD)
qdate  :: Parser LocalTime
qdate  = createDate <$> takeTill (== ',')
         where defaultDate = LocalTime (fromGregorian 0001 01 01) (TimeOfDay 00 00 00 )
               parseDateText t = parseTimeM True Data.Time.defaultTimeLocale (iso8601DateFormat Nothing) (unpack t)
               createDate x = fromMaybe defaultDate $ parseDateText x

-- parser for stock quotes
quoteParser :: Text -> Either String Quote
quoteParser = parseOnly quote

-- unit testing Text input string
testString :: Text
testString = "2018-08-05,ASX,100,23.4,25.6\n"

-- unit testing Quote
testQuote :: Quote
testQuote = Quote {
                    qDate   = LocalTime (fromGregorian 2018 08 05) (TimeOfDay 00 00 00),
                    qStock  = "ASX",
                    qShares = 100.0,
                    qBasis  = 23.4,
                    qPrice  = 25.6
                  }

