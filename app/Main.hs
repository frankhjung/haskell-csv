{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           CSV (quoteParser, testString)

--
-- MAIN demo functions
--
main :: IO ()
main = print $ quoteParser testString
