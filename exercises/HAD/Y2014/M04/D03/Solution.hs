module HAD.Y2014.M04.D03.Solution where

import Control.Applicative ((<*>))

-- | foo
-- Types. Powerful enough to get it right.
--
foo :: (a ->  b) -> [a] -> [(a,b)]
foo = (zip <*>) . map
