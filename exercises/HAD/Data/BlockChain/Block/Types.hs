module Data.BlockChain.Block.Types where

-- we keep the common types, used by BlockChain types, here

type Hash = String

-- MANY block chain types have a hashed value. It becomes onerous to come up
-- with new names for each of these values so:

class Sheesh a where
   hash :: a -> Hash

-- Name 'Sheesh' goes with hashed-types, see? hashSheesh!
-- I'll leave now.

-- Motivated to add type by @MadameRecluse
