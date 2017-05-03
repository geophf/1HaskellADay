module Y2017.M05.D03.Solution where

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

{--
We can roll these guards into the solver itself

sisterAge :: Age -> Bool
sisterAge = (< 21)

aprilsAge, maysAge :: (Sister, Age) -> (Sister, Age) -> Age
aprilsAge (june, jage) (may, mage) = jage - 14
maysAge (june, jage) (april, aage) = aage + 3
--}

sistersAges :: [Array Sister Age]
sistersAges = [1..6] >>= \apr ->
   let may = apr + 3
       june = apr + 14

-- since June is 14 years older than April, and all ages must be less than
-- 21, April cannot be older than 6, so we range over only values 1 .. 6.

   in  guard (june == may * 2 + 2) >>
       return (listArray (April, June) [apr, may, june])

{--
>>> sistersAges 
[array (April,June) [(April,6),(May,9),(June,20)]]
--}
