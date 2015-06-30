{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Diagrams.Backend.SVG
import           Diagrams.Prelude          hiding (end, height, width)
-- import qualified Diagrams.TwoD.Path.Metafont as MF
import qualified Options.Applicative       as O
import qualified Options.Applicative.Types as O
import           Prelude                   hiding (length, reverse)

-- units are taken to be inches if this were built to full design scale

-- thickness, shelfHeight, shelfLength, depth :: Double
-- thickness = 0.125*12 -- plywood thickness
-- shelfHeight = 14 -- shelf height
-- shelfLength = 36
-- depth = 12

main :: IO ()
main = do
  config <- O.execParser helpParser
  -- TODO calculate SVG dims from output dims
  renderSVG (filename config) (dims $ mkR2 400 400) (boxenPieces config # pad 1.02)

data Config = Config
              { filename     :: FilePath
              , width        :: Double
              , length       :: Double
              , height       :: Double
              , thickness    :: Double
              , notches      :: Maybe (Int, Int, Int)
              , fingerLength :: Maybe Double
              , clearance    :: Double
              , margin       :: Double
              }

cliParser :: O.Parser Config
cliParser = Config
            <$> O.strOption (O.value "fingerjointed-box.svg" <> O.long "output"
                             <> O.short 'o' <> O.help "filename for output"
                            <> O.showDefault )
            <*> O.option O.auto (O.long "width" <> O.short 'w'
                               <> O.help "width of box")
            <*> O.option O.auto (O.long "length" <> O.short 'l'
                               <> O.help "length of box")
            <*> O.option O.auto (O.long "height" <> O.short 'h'
                               <> O.help "height of box")
            <*> O.option O.auto (O.long "thickness" <> O.short 't'
                               <> O.help "thickness of material / depth of notches")
            <*> O.option (Just <$> O.auto) (O.value Nothing <> O.long "notches" <> O.short 'n'
                              <> O.help "The number of notch pairs in each direction")
            <*> O.option (Just <$> O.auto) ( O.value Nothing
                                           <> O.long "finger" <> O.short 'f'
                                           <> O.help "target length of each finger")
            <*> O.option O.auto (O.value 0 <> O.long "clearance" <> O.short 'c'
                                 <> O.help "extra clearance in joints" <> O.showDefault)
            <*> O.option O.auto (O.value 1 <> O.long "margin" <> O.short 'm'
                                 <> O.showDefault
                                 <> O.help "space between seperate pieces to be cut.  You likely want to rearrange the pieces anyway, to better fit the raw material.")
            -- <*> pure Nothing <*> pure Nothing <*> pure 0 <*> pure 1

helpParser :: O.ParserInfo Config
helpParser = O.info (O.helper <*> cliParser)
             (O.fullDesc
             <> O.header "boxen - finger jointed boxes of any dimensions, for laser cutting or routing"
             <> O.progDesc "The --length, --width, --height, and --thickness arguments are required.  All others are optional.")

boxenPieces :: Config -> Diagram SVG
boxenPieces config = vcat [ side
                          , strutY space
                          , middleRow # centerXY
                          , strutY space
                          , side # reverse
                          , strutY space
                          , top
                          ] # centerXY where
  -- side where
  middleRow, side, end, top :: Diagram SVG
  middleRow = hcat [end, strutX space, base, strutX space, end # reverse]
  ns = notchCalc config
  -- end = centerXY $ (centerY . alignL . strokeTrail) t <> posHandle where
  --   t = depthNotches <> heightNotches <> reverse depthNotches <> heightStraight
  space = margin config
  --   posHandle = translate space . centerY . alignL . rotateBy (-0.25) $ handle
  end = centerXY . strokeTrail . mconcat $
        [ notchTrail (nWidth ns) yDir, notchTrail (nHeight ns) xDir
        , notchTrail (nWidth ns) _yDir, notchTrail (nHeight ns) _xDir ]

  side = centerXY . strokeTrail $ t where
    t = mconcat [notchTrail (nLength ns) xDir , notchTrail (nHeight ns) yDir
                , notchTrail (nLength ns) _xDir, notchTrail (nHeight ns) _yDir ]

  base = centerXY . strokeTrail . mconcat $
                [ notchTrail (nLength ns) xDir, notchTrail (nWidth ns) yDir
                , notchTrail (nLength ns) _xDir, notchTrail (nWidth ns) _yDir ]
  top = centerXY $ rect (length config) (width config)

-- boxenDrawn :: Diagram SVG R2
-- boxenDrawn = vcat [ side config # alignR
--                   , strutY 1
--                   , middleRow # alignR
--                   ] # lw 0.05 # centerXY where
--   middleRow = hcat [end config, strutX 1, base config]

notch :: (Vn t ~ V2 Double, TrailLike t) => Config -> t
notch config = fromOffsets [ -l *^ unitY
                        , t *^ unitX
                        , l *^ unitY
                        , t *^ unitX
                        ] where
  l = length config
  t = thickness config

-- a cut-through handle
-- handle :: Diagram SVG R2
-- handle = either (error.show) id $
--          MF.fromString --"(-3,0){1,0}..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..cycle"
--          "(0,0)..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..(-3,0){1,0}..cycle"

-- | notchesDividing d n v places n notches of depth d along v so that
-- they evenly divide the length.

-- notchesDividing ::  (V t ~ R2, TrailLike t, Transformable t) =>
--                     Double -> Int -> R2 -> t
-- notchesDividing d n v = rotate a . trailLike . flip at origin . mconcat . replicate n $ unit where
--   unit = notch d (magnitude v / realToFrac n / 2) :: Trail R2
--   a = direction v :: Turn

-- | notchesLessThan d l v places n notches of depth d along v so that
-- they evenly divide the length, with n chosen as the largest integer
-- such that |v|/n < d

-- notchesLessThan ::  (V t ~ R2, TrailLike t, Transformable t) =>
--                     Double -> Double -> R2 -> t
-- notchesLessThan d l v = notchesDividing d n v where
--   n = ceiling $ magnitude v / l / 2

-- notchesMoreThan ::  (V t ~ R2, TrailLike t, Transformable t) =>
--                     Double -> Double -> R2 -> t
-- notchesMoreThan d l v = notchesDividing d n v where
--   n = floor $ magnitude v / l / 2

-- Many heuristics are possible to pick a notch length.  I think boxes
-- look better if all the notches along one side are the same length,
-- and if the various sides have similar-size notches.  Too few
-- notches doesn't look good, either.  notchLength follows this
-- heuristic, while allowing the user to override either the exact
-- notch count or the length to aim for.

data Notches = Notches
               { nLength :: Joint
               , nWidth  :: Joint
               , nHeight :: Joint
               } deriving (Show, Read)

notchCalc :: Config -> Notches
notchCalc config = case notches config of
  Nothing -> Notches (fit length) (fit width) (fit height) where
    fit dim = Joint (dim config / fromIntegral n / 2) (thickness config) n where
      n = round $ dim config / l
    l = case fingerLength config of
      Nothing -> minimum [length config, width config, height config] / 2
      Just l' -> l'
  Just (nl, nw, nh) -> Notches (fit length nl) (fit width nw) (fit height nh)
    where
      fit dim n = Joint (dim config / fromIntegral n / 2) (thickness config) n

data Joint = Joint
             { forward :: Double
             , depth   :: Double
             , count   :: Int
             } deriving (Show, Read)

notchPair :: (TrailLike t, Vn t ~ V2 Double) => Joint -> Direction V2 Double -> t
notchPair joint dir = fromOffsets
                  [ forward joint *^ fromDirection dir
                  , depth joint *^ rotateBy 0.25 (fromDirection dir)
                  , forward joint *^ fromDirection dir
                  , depth joint *^ rotateBy 0.75 (fromDirection dir)
                  ]

-- | Direction is not part of Joint because the direction is in 2D,
-- according to how the panel is drawn, not the 3D orientation of the
-- assembled box.
notchTrail :: (TrailLike t, Vn t ~ V2 Double, Monoid t) => Joint -> Direction V2 Double -> t
notchTrail joint dir = mconcat $ replicate (count joint) (notchPair joint dir)

-- lengthNotches, heightNotches, depthNotches :: Trail R2
-- lengthNotches = notchesMoreThan thickness 3 $ (shelfLength - 2 * thickness) *^ unitX

-- heightNotches = notchesMoreThan thickness 3 $ unitY ^* (shelfHeight - 2*thickness)

-- depthNotches = notchesMoreThan thickness 3 $ unitX ^* (depth - thickness)

-- lengthStraight, heightStraight :: Trail R2
-- lengthStraight = fromOffsets [unit_X ^* (shelfLength - 2*thickness)]

-- heightStraight = fromOffsets [unit_Y ^* (shelfHeight - 2*thickness)]

reverse :: (Transformable t, V t ~ V2, Floating (N t)) => t -> t
reverse = rotateBy 0.5

_xDir = reverse xDir
_yDir = reverse yDir

instance Semigroup (O.Mod f a) where
  (<>) = mappend

instance Semigroup (O.InfoMod a) where
  (<>) = mappend
