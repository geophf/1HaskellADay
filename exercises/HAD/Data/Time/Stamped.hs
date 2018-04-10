module Data.Time.Stamped where

-- A protocol to add a timestamp to values

import Control.Monad.Writer

import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

-- below imports available via 1HaskellADay git repository

import Control.DList (DList, dl')
import Data.Time.Now (now)

data Stamped a = Stamped { stamped :: a, time :: LocalTime }
   deriving Show

stampIt :: a -> IO (Stamped a)
stampIt e = Stamped e <$> now

-- takes a log entry and puts the current timestamp on it

type StampedWriter a = WriterT (DList (Stamped a)) IO
         
-- sayIO may be an useful function to define as well:
         
sayIO :: a -> StampedWriter a ()
sayIO entry = lift (dl' <$> stampIt entry) >>= tell
