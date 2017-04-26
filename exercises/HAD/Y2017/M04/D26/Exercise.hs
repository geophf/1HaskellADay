module Y2017.M04.D26.Exercise where

{--
So, there's an achievement for taking out a baddie at 400 meters with a sniper
rifle in Ghost Recon, the Final Ghost Recon, XVIII

(that's not the title of the game, but it's something like that.)

Well, they include PGP: 'pretty good physics.'

So, if a sniper rifle round has a muzzle velocity of 860 m/s then the velocity
decreases as per https://en.wikipedia.org/wiki/Sniper_equipment then how many
degrees do you have to adjust upward from the target to hit it, given level
ground?
--}

type Range = Float
type Velocity = Float

sniperRangeVelocity :: FilePath -> [(Range, Velocity)]
sniperRangeVelocity wikilink = undefined

type Acceleration = Float

-- you can also save the grid locally if that helps

gravity :: Acceleration
gravity = undefined

type Angle = Float

angleOfAttack :: [(Range, Velocity)] -> Acceleration -> Range -> Angle
angleOfAttack velocities gravity range = undefined
