{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Diagrams.Prelude hiding (end)
import Diagrams.Backend.SVG
import qualified Diagrams.TwoD.Path.Metafont as MF

-- units are taken to be inches if this were built to full design scale
-- TODO parameterize to separate overall dimensions from notch dimensions

thickness, shelfHeight, shelfLength, depth :: Double
thickness = 0.125*12 -- plywood thickness
shelfHeight = 14 -- shelf height
shelfLength = 36
depth = 12

boxenPieces :: Double -> Diagram SVG R2
boxenPieces space = vcat [ side
                         , strutY space
                         , middleRow # centerXY
                         , strutY space
                         , side # rotateBy 0.5
                         ] # lw 0.05 # centerXY where
  middleRow = hcat [end, strutX space, base, strutX space, end # rotateBy 0.5]

boxenDrawn :: Diagram SVG R2
boxenDrawn = vcat [ side # alignR
                  , strutY 1
                  , middleRow # alignR
                  ] # lw 0.05 # centerXY where
  middleRow = hcat [end, strutX 1, base]

notch :: (V t ~ R2, TrailLike t) => Double -- ^ depth
                                 -> Double -- ^ length
                                 -> t
notch d l = fromOffsets [ -d *^ unitY
                        , l *^ unitX
                        , d *^ unitY
                        , l *^ unitX
                        ]

handle :: Diagram SVG R2
handle = either (error.show) id $
         MF.fromString --"(-3,0){1,0}..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..cycle"
         "(0,0)..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..(-3,0){1,0}..cycle"

-- | notchesDividing d n v places n notches of depth d along v so that
-- they evenly divide the length.
notchesDividing ::  (V t ~ R2, TrailLike t, Transformable t) =>
                    Double -> Int -> R2 -> t
notchesDividing d n v = rotate a . trailLike . flip at origin . mconcat . replicate n $ unit where
  unit = notch d (magnitude v / realToFrac n / 2) :: Trail R2
  a = direction v :: Turn

-- | notchesLessThan d l v places n notches of depth d along v so that
-- they evenly divide the length, with n chosen as the largest integer
-- such that |v|/n < d
notchesLessThan ::  (V t ~ R2, TrailLike t, Transformable t) =>
                    Double -> Double -> R2 -> t
notchesLessThan d l v = notchesDividing d n v where
  n = ceiling $ magnitude v / l / 2

notchesMoreThan ::  (V t ~ R2, TrailLike t, Transformable t) =>
                    Double -> Double -> R2 -> t
notchesMoreThan d l v = notchesDividing d n v where
  n = floor $ magnitude v / l / 2

lengthNotches, heightNotches, depthNotches :: Trail R2
lengthNotches = notchesMoreThan thickness 3 $ (shelfLength - 2 * thickness) *^ unitX

heightNotches = notchesMoreThan thickness 3 $ unitY ^* (shelfHeight - 2*thickness)

depthNotches = notchesMoreThan thickness 3 $ unitX ^* (depth - thickness)

lengthStraight, heightStraight :: Trail R2
lengthStraight = fromOffsets [unit_X ^* (shelfLength - 2*thickness)]

heightStraight = fromOffsets [unit_Y ^* (shelfHeight - 2*thickness)]

end, side, base :: Diagram SVG R2
end = centerXY $ (centerY . alignL . strokeTrail) t <> posHandle where
  t = depthNotches <> heightNotches <> rotateBy 0.5 depthNotches <> heightStraight
  space = 0.75 *^ unitX
  posHandle = translate space . centerY . alignL . rotateBy (-0.25) $ handle

side = centerXY . strokeTrail $ t where
  t = lengthNotches <> rotateBy 0.25 depthNotches <> lengthStraight <> rotateBy 0.75 depthNotches

base = centerXY . strokeTrail $ t where
  t = lengthNotches <>
      heightNotches <> rotateBy 0.5 lengthNotches <> rotateBy 0.5 heightNotches

-- base = centerXY . strokeTrail $ t where
--   t = lengthNotches <> heightNotches

main :: IO ()
main = renderSVG "book boxen.svg" (Dims 400 400) (boxenPieces thickness # pad 1.02)
