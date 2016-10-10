module Data.BlockChain.Block.Utils where

-- A set of utility functions to help formatting block information

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime

-- below import available from 1HaskellADay git repository

import Data.Monetary.BitCoin

-- Converting POSIX int to LocalTime (my timezone. You can get your own)

est2time :: Integer -> LocalTime
est2time = int2time est

-- Okay, okay! Here's the generic form:

int2time :: TimeZone -> Integer -> LocalTime
int2time tz = utcToLocalTime est . posixSecondsToUTCTime . fromIntegral

-- and MY timezone. YOU can get your own!

est :: TimeZone
est = TimeZone (negate 300) False "EST"

-- Converting 'value'-value (big-int) to BTC

val2BTC :: Integer -> BitCoin
val2BTC = BTC . (/ 100000000) . fromIntegral

-- *Y2016.M09.D23.Solution> val2BTC 1302390887 ~> BTC 13.02
