module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = 4/3 * radius^3 * pi

sphereArea :: Float -> Float
sphereArea radius = 4*pi*radius^2

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume h w d = h*w*d

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea h w d = (h*w + w*d + h*d)*2
