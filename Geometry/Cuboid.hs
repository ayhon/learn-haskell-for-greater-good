module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float 
volume h w d = h*w*d

area :: Float -> Float -> Float -> Float
area h w d = 2*(h*w + w*d + h*d)
