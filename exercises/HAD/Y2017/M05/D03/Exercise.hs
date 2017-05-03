module Y2017.M05.D03.Exercise where

import Control.Monad (guard)
import Data.Array

{--
From Mensa Genius Quiz-a-Day Book, by Dr. Abbie F. Salny, et al.

Ages seem to be perennially fascinating to puzzle writers, so why fight it?

Of three sisters named April, May, and June, none is yet twenty-one.

April is now as old as June was fourteen years ago,
and two-thirds of May's age.

May, on the other hand, will be June's age when May is twice as old as she is
now plus two years.

Three years ago, May was as old as April is now.

How old are April, May, and June?
--}

data Sister = April | May | June
   deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type Age = Int

sisterAge :: Age -> Bool
sisterAge = (< 21)

aprilsAge, maysAge :: (Sister, Age) -> (Sister, Age) -> Age
aprilsAge (june, jage) (may, mage) = undefined
maysAge (june, jage) (april, aage) = undefined

sistersAges :: Monad m => m (Array Sister Age)
sistersAges = undefined
