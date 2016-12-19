module Y2016.M12.D19.Exercise where

import Codec.Compression.GZip

-- below imports available at the 1HaskellADay git repository

import Data.SymbolTable
import Data.SymbolTable.Compiler

{--
Today and this week we're going to be focusing on the SAIPE/poverty data that
we started to look at last week. But we'll be breaking this examination into
daily bite-sized pieces, because I'm all nice like that.

So, last week I asked for a set of ScoreCards from the data set, but score-cards
are based on two sets of indices, one for the score card and one for each datum
of the (indexed) arrayed data set for the score car.


A county makes a perfect index for a score card, as each row is data on the 
county. One small problem: a String is not an Ix type.

That's a problem.

Another, semi-related/unrelated problem is that an uniquely identified County
is either: not a string, or if it is, it embeds, then loses, the State in which
it is.

Huh?

("Middlesex County","CT") is not a String, and the original String:

"Middlesex County (CT)" becomes a parsing problem with the embedded State. I
grant you, it's an uninteresting parsing problem for you grizzled ancients,
but a datum, fundmentally, should be atomic. This 'datum' is a cartesian product.

Not good.

Well, if you've been following along, we solved the parsing problem last week...

YAY!

But we still need a set of unique indices for each score cards (1) and (2) we'd
like to retain, and not lose, the State to which this County belongs.

Today's Haskell exercise, then is a rather simple problem.

As we saw last week, following along with this example, the State (NOT the
StateAbbrev, but the USState) is Connecticut, and we 'know' this because of
indirect structural information of the SAIPE data file (which is here:

Y2016/M12/D15/SAIPESNC_15DEC16_11_35_13_00.csv.gz

in this git repository) where the Connecticut-row preceeds the Middlesex County
(CT)-row.

Okay.

Today, even simpler than the 'Which State countains Middlesex County (CT)?'-
question, is this request:

from the SAIPE data file, create a Data.SAIPE.USStates module that enumates
each USState as a value as a USStates data type, i.e.:

Read in SAIPESNC_15DEC16_11_35_13_00.csv.gz and output the module

module Data.SAIPE.USStates where

import Data.Array

data USStates = Alabama | Arizona | ...
   deriving (Eq, Ord, Show, Read, Enum, Ix)

Hints: the above imports may help. Determining is a US State and what is not
is up to you, but a hint here is that we have examined how to make this
determination by context in exercises last week. Haskell provides a gzip
reader, provided in the above import.
--}

usStateIndices :: FilePath -> FilePath -> IO ()
usStateIndices gzipSAIPEdata outputfilename = undefined

-- from the gzipped SAIPE data set output the enumerated USStates as the
-- Data.SAIPE.USStates module
