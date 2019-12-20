module Main where

import Data.Bifunctor

data Moon = Moon (Int, Int) (Int, Int) (Int, Int)

instance Show Moon where
  show (Moon (px,vx) (py,vy) (pz,vz))
    =  "Pos: (" <> show px <> "," <> show py <> "," <> show pz <> ")\t"
    <> "Vel: (" <> show vx <> "," <> show vy <> "," <> show vz <> ")"

type System = [Moon]

gravity :: System -> System
gravity system = map (baz system) system
  where
    baz :: System -> Moon -> Moon
    baz sys moon = foldr go moon (map (bar moon) sys)

    go :: ((Int->Int),(Int->Int),(Int->Int)) -> Moon -> Moon
    go (fx,fy,fz) (Moon x y z) = Moon (second fx x) (second fy y) (second fz z)

    bar :: Moon -> Moon -> ((Int->Int),(Int->Int),(Int->Int))
    bar (Moon x1 y1 z1) (Moon x2 y2 z2) =
      let dx = foo x1 x2
          dy = foo y1 y2
          dz = foo z1 z2
      in (dx,dy,dz)

    foo :: (Int,Int) -> (Int,Int) -> (Int -> Int)
    foo (px, vx) (py, vy)
      | px == py = id
      | px >  py = pred
      | px <  py = succ

step :: System -> System
step = map go
  where
    go :: Moon -> Moon
    go (Moon (px,vx) (py,vy) (pz,vz)) =
      Moon (px+vx,vx) (py+vy,vy) (pz+vz,vz)

energy :: System -> Int
energy = sum . map go
  where
    go :: Moon -> Int
    go (Moon (px,vx) (py,vy) (pz,vz)) =
      (abs px + abs py + abs pz) * (abs vx + abs vy + abs vz)

simulate :: System -> Int -> System
simulate system timesteps = iterate (step . gravity) system !! timesteps

simulateAll :: System -> Int -> [System]
simulateAll system timesteps = take (succ timesteps) (iterate (step . gravity) system)

initMoon :: (Int, Int, Int) -> Moon
initMoon (x,y,z) = Moon (x,0) (y,0) (z,0)

initSystem :: System
initSystem =
  [ initMoon (4,12,13)
  , initMoon (-9,14,-3)
  , initMoon (-7,-1,2)
  , initMoon (-11,17,-1)
  ]

type SimpleSystem = [SimpleMoon]
type SimpleMoon = (Int, Int)

simpleStep :: SimpleSystem -> SimpleSystem
simpleStep = map (\(p,v) -> (p+v,v))

simpleGravity :: SimpleSystem -> SimpleSystem
simpleGravity system = map (accelerate system) system
  where
    accelerate :: SimpleSystem -> SimpleMoon -> SimpleMoon
    accelerate system moon = foldr second moon (map (foo moon) system)

    foo :: SimpleMoon -> SimpleMoon -> (Int -> Int)
    foo (px, vx) (py, vy)
      | px == py = id
      | px >  py = pred
      | px <  py = succ

stopSame :: SimpleSystem -> Int
stopSame system = go system system 0
  where
    go :: SimpleSystem -> SimpleSystem -> Int -> Int
    go current initSystem counter =
      let next = simpleStep (simpleGravity current)
      in 
        if next == initSystem
        then counter + 1
        else go next initSystem (counter + 1)

xs :: SimpleSystem
xs = [(4,0),(-9,0),(-7,0),(-11,0)]

ys :: SimpleSystem
ys = [(12,0),(14,0),(-1,0),(17,0)]

zs :: SimpleSystem
zs = [(13,0),(-3,0),(2,0),(-1,0)]

main :: IO ()
main = do
  print (energy $ simulate initSystem 1000)
  let x = stopSame xs
      y = stopSame ys
      z = stopSame zs
  print (lcm x (lcm y z))
