module Data.Percentage where

import Control.Presentation

data Percentage = P { percent :: Rational }
   deriving (Eq, Ord) 

instance Show Percentage where
   show = (++ "%") . rep
instance Raw Percentage where
   rep = laxmi 2 . (100 *) . percent
