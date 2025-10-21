module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "aralex-backend" $ do
    it "compiles successfully" $ do
      True `shouldBe` True

    it "can parse dictionary entries" $ do
      pending  -- TODO: Add parser tests

    it "can parse morphology segments" $ do
      pending  -- TODO: Add morphology tests

    it "can parse verse text" $ do
      pending  -- TODO: Add verse tests
