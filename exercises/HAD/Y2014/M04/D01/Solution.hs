module HAD.Y2014.M04.D01.Solution where

data Binary
  = A Binary
  | B Binary
  | Done
  deriving (Eq, Read, Show)

-- | isCyclic Check if the Binary object is cyclic
--
-- Examples
-- >>> isCyclic $ A $ B $ Done
-- False
--
-- >>> :{
--   let
--     x = A y
--     y = B x
--   in isCyclic x
-- :}
-- True
--
isCyclic :: Binary -> Bool
isCyclic = undefined -- So long, and thanks for all the fish
