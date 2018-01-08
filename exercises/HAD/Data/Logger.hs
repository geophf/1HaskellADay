module Data.Logger where

-- Logging functionality

import Control.Monad.Writer

-- below imports available via 1HaskellADay git repository

import Control.DList

type Logger m a = WriterT (DList String) m a

say :: Monad m => String -> Logger m ()
say = tell . dl'

-- and that's all you need for generic logging functionality ... although
-- we can add a date-stamp and logging levels (debug, info, critical, error)
