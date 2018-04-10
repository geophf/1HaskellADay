module Y2018.M04.D10.Exercise where

-- What time is it right now? That's today's Haskell exercise

import Data.Time.Clock
import Data.Time.LocalTime

now :: IO LocalTime
now = undefined

-- hint: use library functions from both modules to get the current time (UTC)
-- and convert that value into a LocalTime value.
