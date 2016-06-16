module Data.Percentage where

-- http://lpaste.net/4915955460319739904

import Control.Presentation          -- http://lpaste.net/588030780018524160

data Percentage = P { percent :: Rational }
   deriving (Eq, Ord) 

instance Show Percentage where
   show = (++ "%") . rep
instance Raw Percentage where
   rep (P p) = laxmi 2 p
