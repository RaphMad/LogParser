module Main where

import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8
import LogParser
import Test.Hspec
import Test.Hspec.Attoparsec

main :: IO ()
main = hspec $ do

   describe "date parser" $ do

      it "succeeds on correct date" $
         date `succeedsOn` "04.01.2016"

      it "fails on wrong date" $
         date `failsOn` "04.error.2016"

   describe "time parser" $ do

      it "succeeds on correct time" $
         time `succeedsOn` "04:04:54,407"

      it "fails on wrong time" $
         time `failsOn` "04:04error,407"

   describe "timeStamp parser" $ do

      it "succeeds on correct dateTime" $
         timeStamp `succeedsOn` "04.01.2016 04:04:54,407"

      it "succeeds on correct dateTime with appendix" $
         timeStamp `succeedsOn` "04.01.2016 04:04:54,407appendix"
         
      it "fails on wrong dateTime" $
         timeStamp `succeedsOn` "04.01.2016error 04:04:54,407"

succeedsOn :: Show a => Parser a -> String -> Expectation
succeedsOn p s = p `shouldSucceedOn` pack s

failsOn :: Show a => Parser a -> String -> Expectation
failsOn p s = p `shouldFailOn` pack s
