module Y2021.M03.D19.Solution where

{--
@AnecdotesMaths has been publishing a formula for pi, or to approximate pi,
every day this month. I've been retweeting those formulae, because I like pi.

I also like tau, and I also like cake, so there's that.

Take one of those formulae and compute pi to a 'reasonable' degree of accuracy.

Print pi.

I choose Beeler's method:

pi / 2 = 1/1 + 1/3 + 1x2/3x5 + 1x2x3/3x5x7 ...
--}

import Prelude hiding (pi)

pi :: Double -- or some other numerical representation that suits you.
pi = nthPi 100

{-- BONUS -------------------------------------------------------

The above formula is either inaccurate or does not terminate ... or both.

Have a formula that either stops after the nth iteration or stops within a
certain degree of accuracy.
--}

nthPi :: Int -> Double
nthPi n = 2 * quarterTau (pred n) 1 [1] [3]

quarterTau :: Int -> Double -> [Double] -> [Double] -> Double
quarterTau 0 pi _ _ = pi
quarterTau n pi' num@(nu:_) dem@(d:_) =
   quarterTau (pred n) (pi' + (product num / product dem))
              (succ nu:num) (d+2:dem)

piWithin :: Double -> Double
piWithin epsilon = undefined

-- n.b. Today, 3.19 is a 'kinda'-accurate representation of pi? maybe?
