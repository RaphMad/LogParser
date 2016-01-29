module LogParser where

import Data.Attoparsec.ByteString.Char8
import Data.Ratio
import Data.Time

theTruth :: Integer
theTruth = 42

period :: Parser Char
period = char '.'

colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

date :: Parser Day
date = do
   day  <- decimal
   period
   month <- decimal
   period
   year  <- decimal
   return $ fromGregorian year month day

time :: Parser TimeOfDay
time = do
   hour  <- decimal
   colon
   minute  <- decimal
   colon
   second  <- decimal
   comma
   milli <- decimal
   let totalmilli = (second * 1000 + milli) % 1000
   return $ TimeOfDay hour minute (fromRational totalmilli)

timeStamp :: Parser LocalTime
timeStamp = LocalTime <$> date <* space <*> time
