module Data.Monetary.Currency where

import Control.Presentation
import Data.Percentage

-- A type-classical way of uniformly treating monetary values.

class Currency a where value :: a -> Rational

-- Easy Street, that's where I wanna be!

mknMoney :: Currency p => (Rational -> p) -> String -> p
mknMoney mon str = let dd = (0.005 + read str) in mon (toRational dd)

{--
The 0.005 adjustment addresses a conversion of floating points flooring too
low, and it ... 'works.' I'm not sure if it's too generous in some cases,
however, so: caveat emptor.

*Data.Monetary.USD> mknMoney USD "15.51" ~> $15.51
*Data.Monetary.USD> mknMoney USD "15.52" ~> $15.52
*Data.Monetary.USD> mknMoney USD "15.53" ~> $15.53
--}

-- so, what do we to with money, besides count it? We see how much MORE we have!

gain :: Currency m => m -> m -> Percentage
gain base curr = P $ 100 * (value curr - value base) / value base
