module Y2018.M02.D13.Exercise where

{--
Okay, SOME of the articles from yesterday's problem MAY have unicode, and that
plays havok with plain-old String values.

Of course, the solution to that is to use Text values, instead, but will we?

Nooooooooooooo!

Of course not! Instead, we're going to remove any special characters from
the article texts and deal, instead, with just plain old ASCII.
--}

-- below import available via 1HaskellADay git repository

import Y2018.M02.D12.Exercise

asciify :: Article -> Article
asciify art = undefined

-- how many articles from yesterday's data set changed?
