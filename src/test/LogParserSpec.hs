module Main where

import LogParser
import Test.Hspec

main :: IO ()
main = hspec $

   describe "Check arithmetics" $

   it "adds correctly" $
      2 + 3 `shouldBe` 5
