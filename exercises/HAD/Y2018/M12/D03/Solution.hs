{-# LANGUAGE TupleSections #-}

module Y2018.M12.D03.Solution where

{--
The KAYAK puzzle.

A 30 x 30 grid contains a random dispersion of the letters A, K, and Y.

1. generate this grid; print it
2. Does this grid contain the word 'KAYAK' horizontally, vertically, or
   BONUS: diagonally?
3. BONUS: How many occurances of the word 'KAYAK' are in this grid?

Hint: to generate a random number from 1 to 3

>>> randomRIO (1,3)
3
--}

import Control.Arrow (app)

import Data.Array
import Data.List (isInfixOf, stripPrefix)

import System.Random (randomRIO)

type Size = Int
type Grid = Array (Int,Int) Char

aky :: Array Int Char
aky = listArray (1,3) "AKY"

grid :: Size -> IO Grid
grid sz = array ((1,1), (sz,sz)) . concat <$> mapM (flip row sz) [1 .. sz]

{--
>>> grid 3
array ((1,1),(3,3)) [((1,1),'K'),((1,2),'A'),((1,3),'A'),
                     ((2,1),'Y'),((2,2),'K'),((2,3),'K'),
                     ((3,1),'K'),((3,2),'A'),((3,3),'A')]
--}

{-
array ((1,1), (sz,sz)) <$>
   \r -> mapM (const (row r sz)) [1 .. sz]
-}

type Row = Int

row :: Row -> Size -> IO [((Int,Int), Char)]
row r sz = mapM (\c -> toAKY >>= return . ((r,c),)) [1 .. sz]

{--
>>> row 2 5
[((2,1),'Y'),((2,2),'K'),((2,3),'K'),((2,4),'K'),((2,5),'A')]
--}

toAKY :: IO Char
toAKY = (aky !) <$> randomRIO (1,3)

{--
>>> toAKY
'A'
>>> toAKY
'K'
>>> toAKY
'Y'
>>> toAKY
'Y'
--}

printGrid :: Grid -> IO ()
printGrid = pg' 1 . assocs

pg' :: Row -> [((Int, Int), Char)] -> IO ()
pg' _ [] = nl
pg' r (((a,b),c):elts) =
   (if a > r then nl else noop) >> sp >> putChar c >> pg' a elts

sp :: IO ()
sp = putChar ' '

nl :: IO ()
nl = putStrLn ""

noop :: Applicative f => f ()
noop = pure ()

{--
>>> grid 3 >>= printGrid
 Y Y Y
 K Y K
 A Y A
--}

hasKayakinRowOrColumn :: Grid -> Bool
hasKayakinRowOrColumn grid =
   any (isInfixOf "KAYAK") (rows grid ++ cols grid)

rows, cols :: Grid -> [String]
rows = things (thingAsStr fst)
cols = things (thingAsStr snd)

things :: (Row -> Grid -> String) -> Grid -> [String]
things fn g = map app (zip (map fn [1 .. sz g]) (replicate (sz g) g))

sz :: Grid -> Size
sz = snd . snd . bounds

thingAsStr :: ((Int,Int) -> Int) -> Row -> Grid -> String
thingAsStr fn r = map snd . filter ((== r) . fn . fst) . assocs

-- BONUS -------------------------------------------------------

hasKayakinDiagonals :: Grid -> Bool
hasKayakinDiagonals grid =
   any (isInfixOf "KAYAK") (lDiags grid ++ rDiags grid)

lDiags, rDiags :: Grid -> [String]
lDiags grid = map (flip lDiagRow grid) [1 .. sz grid]
           ++ map (flip lDiagCol grid) [2 .. sz grid]
rDiags grid = map (flip rDiagRow grid) [1 .. sz grid]
           ++ map (flip rDiagCol grid) [2 .. sz grid]

lDiagRow, rDiagRow, lDiagCol, rDiagCol :: Row -> Grid -> String
lDiagRow r grid = diagIt ([r .. sz grid], [1 .. sz grid]) grid
rDiagRow r grid = map (grid !) (zip [r, pred r .. 1] [1 .. sz grid])
lDiagCol c grid = map (grid !) (zip [1 .. sz grid] [c .. sz grid])
rDiagCol c grid = map (grid !) (zip [1 .. sz grid] [c, pred c .. 1])

diagIt :: ([Int], [Int]) -> Grid -> String
diagIt (rs,cs) grid = map (grid !) (zip rs cs)

-- eh, diagIt or spell it out ... which way is better?

kayaksCount :: Grid -> Int
kayaksCount grid = 
   sum (map countKayak (rows grid ++ cols grid ++ lDiags grid ++ rDiags grid))

countKayak :: String -> Int
countKayak = ck 0

ck :: Int -> String -> Int
ck ans "" = ans
ck acc str = let substr = str `minus` "KAYAK" in
   if substr == "" then acc else ck (succ acc) substr

minus :: String -> String -> String
minus "" _ = ""
minus str@(_:t) pref = case stripPrefix pref str of
   Nothing -> minus t pref
   Just sommat -> sommat

{--
>>> grid 30 >>= \g ->
    printGrid g >> 
    return (hasKayakinRowOrColumn g,
            map (map countKayak) [rows g, cols g, lDiags g, rDiags g], 
            kayaksCount g)
 Y A K K A Y A K A A Y K K K Y Y A K Y A K A Y Y K K A K K A
 A A K A Y Y Y Y Y A Y A A K A K Y A K K K K A Y Y K Y Y Y A
 K Y K K K Y Y A Y A Y A A A A Y K K Y Y A Y K A A A K K Y Y
 A A Y Y K A K Y A K A K Y Y A K A A A A K Y A A A Y A K A Y
 Y K A K Y A K Y Y Y K Y A A Y A A A A Y K Y Y A Y Y A K A Y
 K A Y Y K A K K Y Y A K K K Y K A Y K Y K K Y K K K A K K A
 A K A A Y K K K K Y A Y K K A K Y K Y Y K K K Y A Y Y Y K A
 A A K Y Y K K Y K Y K Y Y K K K K K A A Y Y A K A A K K A Y
 Y A Y A Y K Y A K Y K Y K A Y A K Y K K Y K K K A Y Y A A Y
 A Y Y K K K A Y A A A A K A K K K A Y K K A K A K K K A K Y
 Y A Y K A K Y A K K K Y K Y Y K Y A Y K K K A A A K A A Y Y
 Y K A Y K A K A K K K Y K A A K Y Y Y Y K A Y K K K K A K Y
 Y K K Y K Y A Y A K K K K Y A Y K Y Y K Y K A A A A Y Y K K
 K Y K Y K A A A A A A A K A Y K K K Y A Y A A Y K Y K K A K
 A A A Y Y A A A Y A Y A Y Y Y Y K A K A Y K A A A A K Y Y Y
 A A A Y K K K A K Y K A Y K K Y K K Y Y K Y A Y A K A A K K
 K Y Y A Y A Y K K K Y K A A K Y K A K K A Y A K A K A K A A
 A A A A K K K Y K K Y A K Y Y A A A Y K K A Y A A Y A K K Y
 K A A K A K Y Y K A A A K Y A K Y K A Y K K A Y Y A Y K K A
 Y Y K Y Y A Y Y K A Y A Y A K Y A K K K A Y K K Y Y K K K K
 Y K K K K A A Y K K A K K A Y A A K A A A A K Y K K Y A A K
 A Y A K K Y A K Y K Y K K K Y A A A A K A A A K K A Y K K K
 K K A K K Y K Y K K A Y A K Y A Y Y A K A A K Y Y K Y K K K
 Y K K A A A Y A K A Y A Y Y K Y Y Y A K K Y Y Y A Y A A Y A
 Y Y Y K K A A A Y K K Y A K Y A K Y A A A A Y Y K Y K Y Y K
 K K A A A Y Y K A Y Y A A K Y Y Y A A A K A K A K Y A K Y Y
 Y Y A K A K K Y A K Y Y Y K Y A K K A K K K K Y K K K K A K
 K Y A K A Y K A K Y A K A K K A Y Y Y K K Y Y Y A A Y K K Y
 K Y K Y K K K Y A Y Y Y A A A Y Y A Y A Y A K A K Y Y Y Y Y
 K A A K K K Y K A Y K A K K A K A Y K A K A K K A Y Y Y K K
(True,[[1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0],
       [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1],
       [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
       [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]],
 9)
--}
