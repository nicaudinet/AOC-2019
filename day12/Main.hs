module Main where

import Data.Bifunctor

data Moon = Moon (Int, Int) (Int, Int) (Int, Int)

instance Show Moon where
  show (Moon (px,vx) (py,vy) (pz,vz))
    =  "Pos: (" <> show px <> "," <> show py <> "," <> show pz <> ") "
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

initMoon :: (Int, Int, Int) -> Moon
initMoon (x,y,z) = Moon (x,0) (y,0) (z,0)

initSystem :: System
initSystem =
  [ initMoon (4,12,13)
  , initMoon (-9,14,-3)
  , initMoon (-7,-1,2)
  , initMoon (-11,17,-1)
  ]

main :: IO ()
main = print (energy $ simulate initSystem 1000)

testSystem :: System
testSystem =
  [ initMoon (-1,0,2)
  , initMoon (2,-10,-7)
  , initMoon (4,-8,8)
  , initMoon (3,5,-1)
  ]

test :: IO ()
test = do
  mapM_ print (gravity testSystem)
  putStrLn "--"
  mapM_ print (step $ gravity testSystem)
  putStrLn "--"
  mapM_ print (simulate testSystem 1)
  putStrLn "--"
  mapM_ print (simulate testSystem 10)
  putStrLn "--"
  print (energy $ simulate testSystem 10)
  putStrLn "--"
  print (energy $ simulate testSystem 100)
