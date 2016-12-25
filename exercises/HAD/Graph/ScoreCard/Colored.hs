module Graph.ScoreCard.Colored where

-- http://lpaste.net/1795812773775540224

import Control.Arrow ((&&&))
import Data.Array

import Control.List (weave)           -- http://lpaste.net/107211
import Data.Graphics.BoundingBox      -- http://lpaste.net/2865245555871711232
import Data.Graphics.Cell             -- http://lpaste.net/6080330641977638912
import Data.Graphics.Color            -- http://lpaste.net/9210044109090193408
import Data.Relation                  -- http://lpaste.net/2242323977663938560
import Graph.ScoreCard                -- http://lpaste.net/7322735479504240640

{-- 
Actually, what to do from here is to convert all the clusters of scorecards to
Cells, and then when we have the cells, we just simply use the colors
--}

data ColoredScoreCard a b c = CSC { sc :: ScoreCard a b c, colour :: Int }
   deriving Show

-- Now let's turn colored score cards into cells so we can display them on an
-- SVG grid:

csc2cell :: Enum a => BoundingBox -> ColoredScoreCard a b c -> Cell a Int
csc2cell = csc2cellWith cell

csc2cellWith :: Enum a => (BoundingBox -> a -> Point2D) ->
                BoundingBox -> ColoredScoreCard a b c -> Cell a Int
csc2cellWith fn bb = uncurry (C <*> fn bb) . (idx . sc &&& recolor . colour)

-- And now that we've got this, we can draw these cells in an SVG grid

-- Okay, all that fun was to incorporate color as a component to the ScoreCard,
-- now, instead of uploading cells to the graph DaaS, we upload the colorized
-- scorecards

instance (Show a, Show b, Ix b, Show c) => Node (ColoredScoreCard a b c) where
   asNode (CSC sc color) = "CELL { idx: \"" ++ show (idx sc) ++ "\", " ++
      weave (map showing (assocs (values sc))) ++ ", color: " 
      ++ showColor color ++ " }"

showing :: (Show a, Show b) => (a,b) -> String
showing (a,b) = show a ++ ": " ++ show b
