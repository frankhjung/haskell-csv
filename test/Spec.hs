import           CSV        (quoteTextParser, testQuote, testString)

import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $

  describe "quoteTextParser" $
    it "returns an instance of Quote" $
      quoteTextParser testString `shouldBe` Right testQuote
