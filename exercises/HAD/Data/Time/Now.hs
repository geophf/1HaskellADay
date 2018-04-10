module Data.Time.Now where

-- What time is it right now? That's today's Haskell exercise

import Data.Time.Clock
import Data.Time.LocalTime

now :: IO LocalTime
now = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

{--
>>> now
2018-04-09 12:32:05.796303

This module is being moved to the 1HaskellADay Data.Time.Now module
--}
