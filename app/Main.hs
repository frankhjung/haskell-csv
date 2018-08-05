{-# LANGUAGE UnicodeSyntax #-}

module Main(main) where

import           CSV

--
-- MAIN demo functions
--
main :: IO ()
main = print $ quoteParser testString
