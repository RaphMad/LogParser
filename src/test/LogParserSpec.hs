module Main where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8
import Data.Time.LocalTime
import LogParser
import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck

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

      it "parses milliseconds exactly" $ property $
         \ms -> (ms :: Int) >= 0 && ms < 1000 ==>
         pack ("04:04:54," ++ show ms) ~> time
         `parseSatisfies` ((== ms) . (`rem` 1000) . truncate  . (* 1000) . todSec)

   describe "timeStamp parser" $ do

      it "succeeds on correct dateTime" $
         timeStamp `succeedsOn` "04.01.2016 04:04:54,407"

      it "succeeds on correct dateTime with appendix" $
         timeStamp `succeedsOn` "04.01.2016 04:04:54,407appendix"

      it "fails on wrong dateTime" $
         timeStamp `failsOn` "04.01.2016error 04:04:54,407"

succeedsOn :: Show a => Parser a -> String -> Expectation
succeedsOn p s = p `shouldSucceedOn` pack s

failsOn :: Show a => Parser a -> String -> Expectation
failsOn p s = p `shouldFailOn` pack s
