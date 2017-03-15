{-# LANGUAGE ViewPatterns #-}

module Analytics.Theory.Statistics.StandardDeviation where

{--
Original established to compute standard deviations against stochastic 
oscillators, but can be applied to any data set with a norm and some variance.
--}

import Control.Arrow ((&&&))
import Data.Array

-- below imports available via 1HaskellADay git repository

import Analytics.Theory.Number.SquareRoot (rSqrt)
import Control.Presentation
import Control.Scan.CSV

{-- You know: wikipedia:

http://en.wikipedia.org/wiki/Standard_deviation

So, we have rows in this CSV-format:

Date,30%,70%,%K,%D,Close
2015-04-28,30,70,60.51,83.10,130.56
2015-04-27,30,70,94.46,92.15,132.64
2015-04-24,30,70,94.32,92.22,130.28
2015-04-23,30,70,87.68,83.99,129.66
...

Now, what we really care about here is %k and %d, so let's get that --}

parseKD :: FilePath -> IO [(Rational, Rational)]
parseKD = 
   fmap (map ((head &&& last)
        . map (\str -> toRational (read str :: Double)) . take 2 . drop 3 . csv)
        . tail . lines) . readFile 

{--
*Main> liftM (take 5) $ parseKD "seer/aapl-analysis.csv" 
[(60.51,83.1),(94.46,92.15),(94.32,92.22),(87.68,83.99),(94.65,82.19)]
--}

data Nat = One | Two | ThreeOn deriving (Eq, Ord, Enum, Bounded, Ix, Show, Read)
  -- complete and consistent ... yeah, right!

data StandardDeviation = SD { n :: Nat, sigma :: Rational } deriving (Eq, Ord)

-- I hate the Show instance of Double, btw

instance Show StandardDeviation where 
   show (SD n rat) = "SD " ++ show n ++ " " ++ laxmi 2 rat
instance Univ StandardDeviation where
   explode (SD nat dist) = [show nat, laxmi 2 dist]
instance Read StandardDeviation where
   readsPrec _ = pure . stddevFromCSVstring

stddevFromCSVstring :: String -> (StandardDeviation, String)
stddevFromCSVstring (csv -> (num:mag:rest)) =
   let doubstep = read mag :: Double
   in  (SD (read num) (toRational doubstep), unwordsBy ',' rest)

-- This sample variance presupposes a computed (moving) variance (snd)
-- if this is not the case, then refactor such that we first compute μ then
-- have μ as snd. Simple enough.

sampleVariance :: Fractional a => [(a,a)] -> a
sampleVariance = (/) . sum . map var <*> fromIntegral . pred . length

var :: Fractional a => (a,a) -> a
var = (^ 2) . uncurry (-)

-- of course, we need µ for the computation, so ...

µ :: Fractional a => [a] -> a
µ = (/) . sum <*> fromIntegral . length

-- *Analytics.Math.Statistics.StandardDeviation> µ [1,3, 9, 6, 4, 7] ~> 5.0

{--
*Main> liftM sampleVariance $ parseKD "seer/aapl-analysis.csv" ~> 177.86
*Main> sqrt it ~> 13.34
--}

-- σ is handed the data set paired with a computed (moving) μ for each datum

σ :: RealFrac a => [(a, a)] -> (a, a) -> StandardDeviation
σ sample kdpoint =
   num2SD (rsqte (var kdpoint) / rsqte (sampleVariance sample))

rsqte :: RealFrac a => a -> a
rsqte = fromRational . flip rSqrt 0.01 . toRational

num2SD :: RealFrac a => a -> StandardDeviation
num2SD = SD . num2nat <*> toRational

num2nat :: RealFrac a => a -> Nat
num2nat dist | dist <= 1.0 = One
             | dist <= 2.0 = Two
             | otherwise   = ThreeOn

exPts :: [(Rational, Rational)]
exPts = [(92.15, 94.46), (83.99, 87.68), (52.59, 30.16)]

{--
*Main> parseKD "seer/aapl-analysis.csv" ~> let aapl = it
*Main> map (σ aapl) exPts ~> [SD One 0.17,SD One 0.27,SD Two 1.68]
--}
