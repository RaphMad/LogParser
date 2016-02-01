{-# LANGUAGE OverloadedStrings #-}

module LogParser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString
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

semicolon :: Parser Char
semicolon = char ';'

date :: Parser Day
date = do
   day <- decimal
   period
   month <- decimal
   period
   year <- decimal
   return $ fromGregorian year month day

time :: Parser TimeOfDay
time = do
   hour <- decimal
   colon
   minute <- decimal
   colon
   second <- decimal
   comma
   millis <- decimal
   let totalmillis = (second * 1000 + millis) % 1000
   return $ TimeOfDay hour minute (fromRational totalmillis)

dateTime :: Parser LocalTime
dateTime = LocalTime <$> date <* space <*> time

data Severity = DEBUG | INFO | WARN | ERROR | FATAL deriving Show

severity :: Parser Severity
severity = string "DEBUG" *> return DEBUG
       <|> string "INFO " *> return INFO
       <|> string "WARN " *> return WARN
       <|> string "ERROR" *> return ERROR
       <|> string "FATAL" *> return FATAL

type ProcessId = Integer

processId :: Parser ProcessId
processId = char 'P' *> decimal

type Module = ByteString

moduleName :: Parser Module
moduleName = takeTill (== ';')

type LogMessage = ByteString

logMessage :: Parser LogMessage
logMessage = takeTill (== '\r')

data LogLine = LogLine LocalTime Severity Integer ByteString ByteString deriving Show

separator :: Parser Char
separator = semicolon <* space

logLine :: Parser LogLine
logLine = LogLine <$> dateTime <* separator
                  <*> severity <* separator
                  <*> processId <* separator
                  <*> moduleName <* separator
                  <*> logMessage
