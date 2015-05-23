module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

main :: IO ()
main = hspec $ describe "Testing haschat" $ do

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)
