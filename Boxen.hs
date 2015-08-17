{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import qualified Debug.Trace               as T

import           Diagrams.Backend.SVG
import           Diagrams.Prelude          hiding (end, height, width)
-- import qualified Diagrams.TwoD.Path.Metafont as MF
import qualified Options.Applicative       as O
import qualified Options.Applicative.Types as O
import           Prelude                   hiding (length, reverse)

main :: IO ()
main = do
  config <- O.execParser helpParser
  renderSized (filename config) $ boxenPieces config # pad 1.02

renderSized :: FilePath -> Diagram SVG -> IO ()
renderSized fp d = renderSVG fp (dims . boxExtents . boundingBox . scale pxPerInch $ d) d

data Config = Config
              { filename     :: FilePath
              , length       :: Double
              , width        :: Double
              , height       :: Double
              , thickness    :: Double
              , notches      :: Maybe (Int, Int, Int)
              , fingerLength :: Maybe Double
              , clearance    :: Double
              , relief       :: Double
              , margin       :: Double
              , hasFeet      :: Bool
              , bottom       :: BottomStyle
              }

-- | Bottom can be joined to sides by finger joints (like the vertical
-- edges) or the bottom can be rabbeted and fit to a dado in the
-- sides.
data BottomStyle = Finger | Rabbet
                 deriving (Read, Show, Eq)

cliParser :: O.Parser Config
cliParser = Config
            <$> O.strOption (O.value "fingerjointed-box.svg" <> O.long "output"
                             <> O.short 'o' <> O.help "filename for output"
                            <> O.showDefault )
            <*> O.option O.auto (O.long "length" <> O.short 'L'
                               <> O.help "length of box")
            <*> O.option O.auto (O.long "width" <> O.short 'W'
                               <> O.help "width of box")
            <*> O.option O.auto (O.long "height" <> O.short 'H'
                               <> O.help "height of box")
            <*> O.option O.auto (O.long "thickness" <> O.short 'T'
                               <> O.help "thickness of material / depth of notches")
            <*> O.option (Just <$> O.auto) (O.value Nothing <> O.long "notches" <> O.short 'n'
                              <> O.help "The number of notch pairs in each direction (length, width, height)")
            <*> O.option (Just <$> O.auto) ( O.value Nothing
                                           <> O.long "finger" <> O.short 'x'
                                           <> O.help "target length of each finger")
            <*> O.option O.auto (O.value 0 <> O.long "clearance" <> O.short 'c'
                                 <> O.help "extra clearance in joints" <> O.showDefault)
               <*> O.option O.auto (O.value 0 <> O.long "relief" <> O.short 'R'
                                    <> O.help "Cut diagonally in the inside corners, so the square outside corners can fit.  The argument is the diameter of the cutting bit.  Generally this is needed when cutting on a CNC router, but not when laser cutting.")
            <*> O.option O.auto (O.value 1 <> O.long "margin" <> O.short 'm'
                                 <> O.showDefault
                                 <> O.help "space between seperate pieces to be cut.  You likely want to rearrange the pieces anyway, to better fit the raw material.")
            <*> O.switch (O.long "feet" <> O.short 'f' <> O.help "add two feet to help align & stack boxes")
            <*> O.option O.auto (O.value Finger <> O.long "bottom" <> O.short 'b' <> O.help "method of attaching box bottom to sides.  Can be Finger or Rabbet" <> O.showDefault)

helpParser :: O.ParserInfo Config
helpParser = O.info (O.helper <*> cliParser)
             (O.fullDesc
             <> O.header "boxen - finger jointed boxes of any dimensions, for laser cutting or routing"
             <> O.progDesc "The --length, --width, --height, and --thickness arguments are required.  All others are optional.")

boxenPieces :: Config -> Diagram SVG
boxenPieces config@(Config { width, length, height, thickness }) = vcat [ side
                          , strutY space
                          , middleRow # centerXY
                          , strutY space
                          , side # reverse
                          , strutY space
                          , top
                          , strutY space
                          , feet
                          ] # lw (0.072 * pure pxPerInch) # centerXY where
  -- side where
  middleRow, side, end, top :: Diagram SVG
  middleRow = hcat [end, strutX space, base, strutX space, end # reverse]
  Notches {nWidth, nLength, nHeight} = notchCalc config
  space = margin config

  end = centerXY . strokeTrail . mconcat $
        -- CCW starting at RHS
        [ notchTrail (nWidth) Notched yDir
        , notchTrail nHeight Notched _xDir
        , fromOffsets [ (width - thickness) *^ unit_Y ]
        , notchTrail nHeight Straight xDir
        ]

  -- CCW starting at bottom
  side = centerXY . strokeTrail $ t where
    t = mconcat
        [ notchTrail nLength Notched xDir
        , notchTrail nHeight Notched yDir
        , fromOffsets [ (length - thickness) *^ unit_X ]
        , notchTrail nHeight Straight _yDir
        ]

  base = outline <> feet
    where
      outline = case bottom config of
        Finger -> centerXY . strokeTrail . mconcat $
                  [ notchTrail nLength Notched xDir
                  , notchTrail nWidth  Notched yDir
                  , notchTrail nLength Notched _xDir
                  , notchTrail nWidth  Notched _yDir
                  ]
        Rabbet -> centerXY $ rect (length  - thickness ) (width - thickness)
      feet = if hasFeet config
             then centerXY $ atPoints (mkP2 <$> [0, x] <*> [0,y]) (repeat mortise)
             else mempty
      mortise = square $ thickness + clearance config
      y = footLength / 2
      x = (length - 4 * thickness)

  top = centerXY $ rect length width
  feet = if hasFeet config
         then centerXY (foot === foot)
         else mempty
    where
      foot = strokeTrail . closeTrail $ fromOffsets
            [ unit_X ^* footLength
            , unit_Y ^* t
            , unitX ^* x
            ] <> tenon
            <> fromOffsets [ unitX ^* (2 * x) ]
            <> tenon
            <> fromOffsets [ unitX ^* x]
      tenon = fromOffsets
              [ unit_Y ^* t
              , unitX ^* t
              , unitY ^* t
              ]
      t = thickness
      x = footLength / 4 - t / 2
  footLength = width - 3 * thickness

-- a cut-through handle
-- handle :: Diagram SVG R2
-- handle = either (error.show) id $
--          MF.fromString --"(-3,0){1,0}..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..cycle"
--          "(0,0)..{1,0}(3,0)..(3,0.75)..(0,2)..(-3,0.75)..(-3,0){1,0}..cycle"

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
    fit dim = Joint (dim config / fromIntegral n / 2) (thickness config) n (relief config) where
      n = round $ dim config / l
    l = case fingerLength config of
      Nothing -> minimum [length config, width config, height config] / 2
      Just l' -> l'
  Just (nl, nw, nh) -> Notches (fit length nl) (fit width nw) (fit height nh)
    where
      fit dim n = Joint (dim config / fromIntegral n / 2) (thickness config) n (relief config)

data PriorEdge = Notched | Straight
             deriving (Show, Read, Eq)

data Corners = Corners
               Bool -- ^ Adjust first segment for prior notch
               Bool -- ^ Adjust last segment for adjacent edge
             deriving (Show, Read, Eq)

data Joint = Joint
             { forward :: Double  -- length per finger
             , depth   :: Double
             , count   :: Int
             , jRelief :: Double
             } deriving (Show, Read, Eq)

notchPair :: (TrailLike t, Vn t ~ V2 Double, Transformable t, Monoid t) =>
             Joint -> Direction V2 Double -> Corners -> t
notchPair Joint {forward, depth, jRelief } dir (Corners adj1 adj2)=
   mconcat $
   [ fromOffsets [ (forward - priorNotch) *^ u
                 , (depth - chordLength) *^ rotateBy 0.25 u]
   , cornerRelief
   , fromOffsets [ (forward - (if adj2 then 1 else 2) * chordLength) *^ u ]
   , if adj2 then mempty else rotateBy 0.75 cornerRelief
   , if adj2 then mempty
     else fromOffsets [ (depth - chordLength) *^ rotateBy 0.75 u ]
   ] where
     u = fromDirection dir
         -- The relief cut is a semicircle; at the (ideal)
         -- inner corner of the joint, the radius bisects
         -- the right angle.
     r = jRelief / 2
     chordLength = if jRelief > 0 then r * sqrt 2 else 0
     priorNotch = if adj1 then depth else 0
     -- jog = (jRelief * 0.5 * (sqrt 2 - 1)) *^ rotateBy 0.375 u
     cornerRelief = if jRelief > 0
                    then scale r $ arc (rotateBy 0.625 dir) (-0.5 @@ turn)
                    else mempty

-- | Direction is not part of Joint because the direction is in 2D,
-- according to how the panel is drawn, not the 3D orientation of the
-- assembled box.
notchTrail :: (TrailLike t, Vn t ~ V2 Double, Transformable t, Monoid t) =>
              Joint -> PriorEdge -> Direction V2 Double -> t
notchTrail joint prior dir = mconcat $
  [ np (prior == Notched) False
  , mconcat $ replicate (count joint - 2) (np False False)
  , np False True
  ] where
    np a b = notchPair joint dir $ Corners a b

reverse :: (Transformable t, V t ~ V2, Floating (N t)) => t -> t
reverse = rotateBy 0.5

_xDir = reverse xDir
_yDir = reverse yDir

instance Semigroup (O.Mod f a) where
  (<>) = mappend

instance Semigroup (O.InfoMod a) where
  (<>) = mappend

pxPerInch :: Double
pxPerInch = 90  -- empirically in Inkscape, though from the Spec I'd expect 96
