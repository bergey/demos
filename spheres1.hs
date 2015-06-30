module Main  where

import           Diagrams.Backend.POVRay
import           Diagrams.Prelude

cam = mm50Camera # translate (r3 (0,0,10))

xy :: Direction V3 Double
xy = direction . r3 $ (-1, -1, -0.5)

light = parallelLight xy white

color :: Colour Double -> Diagram POVRay -> Diagram POVRay
color c = diffuse 0.5 . ambient 0.1 . sc c

example :: Diagram POVRay
example = centerX $ cat unitX
  [sphere # color cyan, sphere # color green, sphere # color blue]

main :: IO ()
main = putStrLn $ renderDia POVRay POVRayOptions $ mconcat [example, cam, light]
