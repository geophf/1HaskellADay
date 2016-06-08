module Control.Logic.Frege where

-- http://lpaste.net/111101

import Control.Arrow
import Control.Monad
import Data.Monoid

infixr 7 <<-
infixr 2 -|

-- provides a logic operator on monoids; all very Fregescque

(-|) :: Monoid m => Bool -> m -> m
True -| m = m
False -| m = mempty

{-- e.g.:

*Control.Logic.Frege> 3 > 4 -| Just "so"
Nothing
*Control.Logic.Frege> 3 > 1 -| Just "so"
Just "so"

A bit more elaborate example:

verify (Rel (B b) (SzQ g) Sibling:rest) ont@(ages, rels) =
   guard (getAll (legit b g rels)) >> verify rest ont
-- TODO: for Older

-- Is the engagement legit? That is, are there no relationships that are
-- incestual?
legit :: Boy -> Girl -> [Couple] -> All
legit brother sister [] = All True
legit brother sister (Rel b g Engaged:rels) =
   (b /= brother -| All (g /= sister)) `mappend` legit brother sister rels

The All-type is from Data.Monoid
--}

-- for the all-too-common f (g a b) pattern:

(<<-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f <<- g = (f .) . g

-- distributing f over a same-kind tupled pair

adjoin :: Arrow a => a b c -> a (b, b) (c, c)
adjoin = join (***)

-- Now for the pattern monad m >>= \x -> guard (p x) >> return x

assert :: MonadPlus m => (a -> Bool) -> a -> m a
assert p x = guard (p x) >> return x

-- cut-operators for Boolean logic.

-- N.B.: cut impacts not the return values but may affect (monadic) effects!

mor, mand :: Monad m => m Bool -> m Bool -> m Bool
mor ma mb = ma >>= \bool -> if bool then ma else mb
mand ma mb = ma >>= \bool -> if bool then mb else ma
