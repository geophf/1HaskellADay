module Data.Logger where

-- Logging functionality

import Prelude hiding (mod)

import Control.Monad.Writer

import qualified Data.Map as Map

-- below imports available via 1HaskellADay git repository

import Control.DList

import Data.LookupTable (LookupTable)

type Logger m a = WriterT (DList LogEntry) m a

say :: Monad m => LogEntry -> Logger m ()
say = tell . dl'

data Severity = TRACE | DEBUG | INFO | WARN | ERROR | FATAL
   deriving (Eq, Ord, Show, Read)

data LogEntry = Entry { sev :: Severity,app,mod,msg :: String }
   deriving (Eq, Show)

data LogEntry' = LE' { ent :: LogEntry, lk :: LookupTable }
   deriving Eq

instance Show LogEntry' where
   show (LE' (Entry sev app mod msg) lk) =
      "Entry' { sev :: " ++ show sev ++ ('/':show (lk Map.! show sev))
          ++ concat (zipWith (\ a b -> ", " ++ a ++ " :: \"" ++ b ++ "\"")
              (words "app mod msg") [app, mod, msg]) ++ " }"

-- and that's all you need for generic logging functionality ... although
-- we can add a date-stamp and logging levels (debug, info, critical, error)

-- We'll also need to add formatting messages for log entries.
