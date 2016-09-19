module Data.Random where

import Control.Arrow
import Control.Monad (liftM)
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Time.Clock.POSIX (getPOSIXTime)

{-- a solution to the problem posted at http://lpaste.net/5973373084290252800

Okay, so! YESTERDAY! We not only discovered that random numbers are monadic-y
but they are also comonadic-y AND they are big.

Like, really big.

As wikipedia warns, random numbers can become consumptive of space or time,
... or both, as we saw in yesterday's approach.

AND Haskell has abandoned RNG, because, you know, legal concerns. 'Legal'
meaning, with random numbers there are so many implicit expectations, and no
real caveats (that are ever read) to warn about pseudo-vs-true- random
numbers and why provide something if all it does is cause strife and heartbreak?

So, today, let's take a step back and implement wikipedia's PGRNG

https://en.wikipedia.org/wiki/Random_number_generation

I lift their definition directly from their page, and I quote:

An example of a simple pseudo-random number generator is the multiply-with-carry
method invented by George Marsaglia. It is computationally fast and has good 
(albeit not cryptographically strong) randomness properties:[7]

m_w = <choose-initializer>;    /* must not be zero, nor 0x464fffff */
m_z = <choose-initializer>;    /* must not be zero, nor 0x9068ffff */

uint get_random()
{
    m_z = 36969 * (m_z & 65535) + (m_z >> 16);
    m_w = 18000 * (m_w & 65535) + (m_w >> 16);
    return (m_z << 16) + m_w;  /* 32-bit result */
}

end-quote.

So, today. Define a random number generator that, unlike yesterday's, is both
fast and efficient, as demonstrated above.

--}

type RNG a = (a,a)

getRandom :: Monad m => StateT (RNG Int) m Int
getRandom = get >>= \(z,w) -> 
   let newz = shiftn 36969 z
       neww = shiftn 18000 w
   in  put (newz, neww) >> return ((newz `shiftL` 16) + neww)

shiftn :: Int -> Int -> Int
shiftn times var = times * (var .&. 65535) + (var `shiftR` 16)

{--
*Data.Random> runState getRandom (12,23) ~> (29074018608,(443628,414000))
*Data.Random> runState getRandom $ snd it ~> (122138587463430,(1863681234,374112006))
*Data.Random> runState getRandom $ snd it ~> (82382507633708,(1257048375,585329708))
*Data.Random> runState getRandom $ snd it ~> (5717141616803,(87229052,498464931))
*Data.Random> runState getRandom $ snd it ~> (1542133998245,(23513615,1145725605))

... yeah. That's random, all right!
--}

-- Now, with that definition, gimme 10, then 100, then 1000 random numbers

gimme :: Monad m => Int -> StateT (RNG Int) m [Int]
gimme 0 = return []
gimme n = liftM2 (:) getRandom (gimme (pred n))

{--
*Data.Random> runState (gimme 10) (12,23) ~>
([29074018608,122138587463430,82382507633708,5717141616803,1542133998245,
  125324673496538,50582964046648,32757674532938,133548713277788,
  157550037520986],
  (2404014256,559239770))
*Data.Random> let (nums, st1) = runState (gimme 100) st ~> yeah
*Data.Random> let (nums, st2) = runState (gimme 1000) st1
*Data.Random> take 3 nums ~>
[15943466568651,7562152527336,98368555867473]

And there you have it! :)
--}

-- exercise: how to generate a _random_ seed of type (Int, Int) for a 
-- random number generator using getPOSIXTime?

halve :: RealFrac a => a -> (Int, Int)
halve = 
   floor >>> flip mod (maxBound :: Int) >>> id &&& habits >>>
   (uncurry shiftR &&& (second (2 ^) >>> uncurry mod))

-- Okay, how do we get the number of bits of a number?

habits :: Int -> Int
habits n = floor (logBase 2 (fromIntegral n)) `div` 2

-- Now, with that definition, partition the POSIX time into halves. Show results

rndSeed :: IO (Int, Int)
rndSeed = liftM (halve . (1000 *)) getPOSIXTime

-- randomness on POSIX time's milli-second

{--
*Main> rndSeed ~> (1387610,851319)
*Main> rndSeed ~> (1387610,853237)
*Main> rndSeed ~> (1387610,858966)
--}

someRNDs :: Monad m => Int -> StateT (RNG Int) m [Int]
someRNDs = gimme

{--
*Data.Random> rndSeed >>= evalStateT (someRNDs 5) ~>
[27564991185027,125554784414212,115981039972,152249042293450,28732271569635]

Those are some big numbers! To limit these to some range, we simply do this:
--}

rndString :: Int -> IO String
rndString len = map letter <$> (rndSeed >>= evalStateT (someRNDs len))

letter :: Int -> Char
letter = chr . (ord 'A' +) . (`mod` 26)

-- *Main> rndString 50 ~> "KPEAUIXBVRWEFASIOKGWYRKKTRJEZLPVVUGKFIEAONPCJEYAZD"

-- So, generate 10 random strings of size n

-- actually, we can't use rndString, as it discards the state which we need
-- to keep the rnd numbers rnd. So we write a helper function that does the
-- same thing, but retains the state

rndStringS :: Monad m => Int -> StateT (RNG Int) m String
rndStringS = liftM (map letter) . someRNDs

{--
*Main> rndSeed >>= runStateT (rndStringS 50) ~>
("ORTUWRQFTLFQXEVTAIAJPWFVWXQMMCJNLNBZRRHYKYYUCUTSKB",(594429940,63982959))

That was easy!

*Data.Random> rndSeed >>= evalStateT (replicateM 10 (rndStringS 50)) >>= mapM_ print ~>
"LLLIXWUSUGFQDVFSXVIEEZSSEANBHEFPKJLMSDQXUYSANIFMMU"
"MVZLPQTHCABVAXZZLTTBJEKEBAWEAECHLXXLRIDOHWBWUHWFYS"
"TVZYXSEPCSTBCPOHTBOPLFHEFMMAKKPFMFONUTCXALWDZGPSMW"
"CZWGWHEFWWFUZAYMSJOIFWXPFJXKGMZEGMDQLVCGMXYCYTHCPP"
"DMMGHWIHPHSTPINALECSLQNTZBKTPZEOWKRTDQZLXNYFNEKJSK"
"TQXJDNFTLOHOFSSYLLCMNBMJZFLXGZMUHPHNLZHYALACZWGNFH"
"NLHAPGKNSZUJXDSTIOKKGDJXDAOOEZMTWKWTHFFUMVTFPFLUVS"
"HWDNGKNMZOKUSSOCYQBPZHMCRVLKMPFWPGWXOHKAONQAQXMPVY"
"DMFKIIWDRKQOBMQQDWQQAYTONWXUHGJHATJEYJHTZOJYQFPERE"
"FGIBGWOJYULLYMJTJRNNUWJIWVTMFFJYZTCMKFDYLENUJQMNDE"
--}
