{-# LANGUAGE TupleSections #-}

module Data.Graphics.BoundingBox where

import Control.Arrow
import Control.Monad
import Data.Foldable

import Control.Logic.Frege ((<<-), adjoin)          -- http://lpaste.net/111101

-- a solution to the problem posted by http://lpaste.net/4021501857770766336

type Index = Int

{-- 
The thing is, we don't wish to keep recomputing the calculations. When a 
bounding box is constructed, we wish to retain the artifacts of construction.
So how I approach this is to make the bounding box a wider type than just its 
bounds and hide the internals in the type (newtype?) (yup)
--}

newtype BoundingBox = Bounder BBI
   deriving Show

type Point2D = (Float, Float)

data BB = BB (Index, Index) Point2D Point2D
   deriving (Eq, Ord, Show)

data BBI = BBinternal BB Int (Float, Float)
   deriving (Eq, Ord, Show)

-- the internal representation is the same as the BoundingBox but the meaning is
-- very different BBI side (lenx/cell, heighty/cell)

makeBoundingBox :: (Index, Index) -> Point2D -> Point2D -> BoundingBox
makeBoundingBox (idxa, idxz) orgs ends =
   -- okay, first, BB is obvs:
   let bb = BB (idxa, idxz) orgs ends
   -- next obvs: the sqrt of indices:
       sz = ceiling $ sqrt $ fromIntegral $ idxz - idxa
   -- now, BBI, not so obvs ... wait: yes it is:
       divr = aside sz
   in Bounder . BBinternal bb sz
              . adjoin divr $ firstsAndSeconds ends orgs

-- ... isn't the last tuple an arrow? or a bifunctor?

makeBBfrom :: Foldable t => t a -> Point2D -> BoundingBox
makeBBfrom = flip makeBoundingBox (0,0) . (0,) . length
    -- #1Liner solution by Gautier DI FOLCO @gautier_difolco

firstsAndSeconds :: (a,b) -> (c,d) -> ((a,c), (b,d))
firstsAndSeconds =
   curry ((fst . fst &&& fst . snd) &&& (snd . fst &&& snd . snd))

-- *Main> makeBoundingBox (1, 100) (0,0) (1000,750) ~>
-- Bounder (BBinternal (BB (1,100) (0.0,0.0) (1000.0,750.0)) 10 (100.0,75.0))

center :: BoundingBox -> Point2D
center (Bounder (BBinternal (BB _ (orgx, orgy) (endx, endy)) _ _)) =
   ((orgx + endx) / 2, (orgy + endy) / 2)

minCellSz :: BoundingBox -> Float
minCellSz (Bounder (BBinternal _ _ (a,b))) = min a b

aside :: Int -> (Float, Float) -> Float
aside count (end, start) = (end - start) / fromIntegral count

-- given that I'm index so-and-so (13, say), what row/column do I fall under?

rowcol :: Enum a => BoundingBox -> a -> (Int, Int)
rowcol (Bounder (BBinternal (BB (start,_) _ _) sq _)) = 
   adjoin succ . flip divMod sq . flip (-) start . fromEnum

-- n.b. row, col is one-based
-- *Main> rowcol bb 13 ~> (2,3)   *Main> rowcol bb 5 ~> (1,5)
-- so there

cell :: Enum a => BoundingBox -> a -> Point2D
cell bb@(Bounder (BBinternal _ _ celsz)) =
   fourStars celsz . adjoin fromIntegral . rowcol bb

fourStars :: Point2D -> Point2D -> Point2D
fourStars = adjoin (uncurry (*)) <<- firstsAndSeconds

{-- 
cells :: [Index]
cells = [46,19,2,52,21,9,77,7,49,42]

*Main> map (flip cell (makeBoundingBox (1,100) (0,0) (1000, 750))) cells ~>
[(500.0,450.0),(200.0,675.0),(100.0,150.0),(600.0,150.0),(300.0,75.0),
 (100.0,675.0),(800.0,525.0),(100.0,525.0),(500.0,675.0),(500.0,150.0)]
--}
