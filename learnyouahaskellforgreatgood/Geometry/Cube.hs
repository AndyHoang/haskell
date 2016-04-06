module Geometry.Cube
  (
  volume,
  area
  )
where

import qualified Geometry.Cuboid as Cuboid
volume :: Float -> Float
cubeVolume side = Cuboid.volume side side side

area :: Float -> Float
cubeArea side = Cuboid.area side side side


