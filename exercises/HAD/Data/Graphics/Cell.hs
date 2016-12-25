module Data.Graphics.Cell where

-- describes a 'cell' of data (representation) in a bounded (graphical) area

import Control.Arrow
import Data.Foldable

-- below imports available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Data.Graphics.BoundingBox
import Data.Graphics.Color
import Data.Graphics.SVG
import Data.XHTML

-- a solution to the problem posted at http://lpaste.net/6809671420902113280

data Cell a b = C a Point2D (Color b) deriving Show

indexedLocation :: Enum a => BoundingBox -> a -> Point2D
indexedLocation = cell

drawCell :: BoundingBox -> Cell a Int -> Element
drawCell bb (C _ centre colour) =
   uncurry circ (adjoin floor centre) (floor (0.5 * minCellSz bb)) (show colour) []

identify :: Show a => BoundingBox -> Cell a Int -> [Element]
identify bb cell@(C idx centre colour) =
   [drawCell bb cell,
    text centre (show idx) (Attrib "fill" (show (complement colour)):font)]

drawCells :: (Foldable t, Show a) => BoundingBox -> t (Cell a Int) -> String
drawCells = drawCellsWith identify

drawCellsWith :: Foldable t => (BoundingBox -> Cell a Int -> [Element])
              -> BoundingBox -> t (Cell a Int) -> String
drawCellsWith fn bb = pprint . svgFromBB bb . concatMap (fn bb)
