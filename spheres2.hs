module Main  where

import           Data.Colour.Palette.ColorSet
import           Diagrams.Backend.POVRay
import           Diagrams.Prelude

cam = mm50Camera # translateZ 40

xy :: Direction V3 Double
xy = direction . r3 $ (-1, -1, -0.5)

light = parallelLight xy white

s = sphere # scaleY 1.6 # translateX 6

color theta = fc $ rybColor (floor $ theta * 24)

example :: Diagram POVRay
example = mconcat
            [transform (aboutZ (t @@ turn)) (s # color t) | t <- [0,1/8..7/8]]

main :: IO ()
main = putStrLn $ renderDia POVRay POVRayOptions $ mconcat [example, cam, light]
