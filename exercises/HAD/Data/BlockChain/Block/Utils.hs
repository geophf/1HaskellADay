module Data.BlockChain.Block.Utils where

-- A set of utility functions to help formatting block information

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime

-- below import available from 1HaskellADay git repository

import Data.Monetary.BitCoin

-- Converting POSIX int to LocalTime (my timezone. You can get your own)

int2time :: Integer -> LocalTime
int2time = utcToLocalTime (TimeZone (negate 300) False "EST")
               . posixSecondsToUTCTime . fromIntegral

-- Converting 'value'-value (big-int) to BTC

val2BTC :: Integer -> BitCoin
val2BTC = BTC . (/ 100000000) . fromIntegral
