{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text  (Text)
import           Data.Time  (LocalTime (..), TimeOfDay (..), fromGregorian)
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Lib        (Quote (..), quoteTextParser)

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

main :: IO ()
main = hspec $

  describe "quoteTextParser" $
    it "returns an instance of Quote" $
      quoteTextParser testString `shouldBe` Right testQuote
