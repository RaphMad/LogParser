module Main where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS hiding (all)
import Data.Either.Unwrap
import Data.Time.LocalTime
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

      it "parses milliseconds exactly" $
         let checkMilli = all (\ms -> flip check (ms :: Int) . parseInput . input $ ms)
             input = BS.pack . ("04:04:54," ++) . show
             parseInput = fromRight . parseOnly time
             check = ((==) . (`rem` 1000) . truncate  . (* 1000) . todSec)
         in [0..999] `shouldSatisfy` checkMilli

   describe "timeStamp parser" $ do

      it "succeeds on correct dateTime" $
         dateTime `succeedsOn` "04.01.2016 04:04:54,407"

      it "succeeds on correct dateTime with appendix" $
         dateTime `succeedsOn` "04.01.2016 04:04:54,407appendix"

      it "fails on wrong dateTime" $
         dateTime `failsOn` "04.01.2016error 04:04:54,407"

succeedsOn :: Show a => Parser a -> String -> Expectation
succeedsOn p s = p `shouldSucceedOn` BS.pack s

failsOn :: Show a => Parser a -> String -> Expectation
failsOn p s = p `shouldFailOn` BS.pack s
