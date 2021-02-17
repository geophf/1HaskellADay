{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D17.Exercise where

{--
Okay, yesterday we ALMOST saved CSV ... the unicode problem. Again.

Because Text -> String of a unicode point

\8212 -> \u8212

worked just fine

But then the Text -> String of this unicode point:

\258 -> \u258

Is incorrect.

The unicode point should be

\u0258

So, that's today's Haskell problem, or as the gecko said in "Hoodwinked!"

"Do everything you did yesterday, but this time: do it good!"
--}

import Y2021.M02.D03.Solution (Review, Review(Review))
import Y2021.M02.D16.Solution hiding (saveUnicodeReviews)

saveUnicodeReviews :: FilePath -> [Review] -> IO ()
saveUnicodeReviews = undefined
