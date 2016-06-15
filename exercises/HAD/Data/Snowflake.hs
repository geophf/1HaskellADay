module Data.SnowFlake where

-- I am a beautiful Snowflake! SoooOoOooooOo pritty!

data SnowFlake = Snow | Flake
   deriving (Eq, Ord, Enum)

pusit :: SnowFlake -> SnowFlake
pusit Snow = Flake
pusit _    = Snow

{--
'Pusit' is the Filipino word for squid. So the joke goes:
"Pusit!" (hand motion of a squid)
"Opposite!" (hand motion of a squid swimming backwards.)

See? Pusit is opposite. Well: the opposite of opposite.
--}

instance Show SnowFlake where
   show Snow  = "_"
   show Flake = "*"

-- Awwwww!
