import           CSV        (quoteParser, testQuote, testString)

import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $

  describe "quoteParser" $
    it "returns an instance of Quote" $
      quoteParser testString `shouldBe` Right testQuote
