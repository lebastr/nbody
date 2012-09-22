import Data.VectorSpace
import Data.AdditiveGroup
import Data.AffineSpace
import Data.List
import Text.Printf
import Control.Monad
import Animate

gamma :: Double
gamma = 1

summ = foldl' (^+^) zeroV

type Force  = (Double, Double)
type Point  = (Double, Double)
type Vector = (Double, Double)
type Mass   = Double
type Time   = Double

data Body = Body { pos  :: Point
                 , mass :: Mass
                 , vel  :: Vector } deriving (Eq)

instance Show Body where
  show b = printf "(%0.4f, %0.4f)" x y
    where (x,y) = pos b

calcForces :: [Body] -> [Force]
calcForces bs = [summ [giveForce a b | b <- bs, b /= a] | a <- bs]

giveForce :: Body -> Body -> Force
giveForce body1 body2 = 
  let m1 = mass body1
      m2 = mass body2
      r  = pos body2 .-. pos body1
      d  = (magnitude r)^3
  in (gamma*m1*m2/d)*^r
  

move :: Time -> [Body] -> [Body]
move dt bodies = map moveBody (zip forces bodies)
  where
    forces = calcForces bodies
    moveBody (f,b) = Body { pos  = p0 .+^ dp
                          , vel  = v0 ^+^ dv
                          , mass = m } where
      m  = mass b
      p0 = pos b
      v0 = vel b
      dv = a^*dt 
      dp = v0 ^* dt ^+^ a ^* (dt^2/2)
      a  = f^/m
      
bodies :: [Body]
bodies = [ Body { pos = (0,0)
                , vel = (0,0)
                , mass = 10 }
         , Body { pos = (0.7,0)
                , vel = (0,2)
                , mass = 1 }]
         
evolution :: Time -> [Body] -> [(Time, [Body])]
evolution dt bs = iterate (\(t0,bs) -> (t0+dt, move dt bs)) (0,bs) 

instance Draw Body where
  draw = draw . pos

main = do
  let evs = evolution 0.001 bodies
  animate $ map snd evs