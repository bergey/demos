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
              }

-- | Bottom can be joined to sides by finger joints (like the vertical
-- edges) or the bottom can be rabbeted and fit to a dado in the
-- sides.
data BottomStyle = Finger | Rabbet

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
                          , strutY space
                          , feet
                          ] # lw (0.072 * pure pxPerInch) # centerXY where
  -- side where
  middleRow, side, end, top :: Diagram SVG
  middleRow = hcat [end, strutX space, base, strutX space, end # reverse]
  ns = notchCalc config
  space = margin config
  end = centerXY . strokeTrail . mconcat $
        [ notchTrail (nWidth ns) yDir, notchTrail (nHeight ns) xDir
        , notchTrail (nWidth ns) _yDir, notchTrail (nHeight ns) _xDir ]

  side = centerXY . strokeTrail $ t where
    t = mconcat [notchTrail (nLength ns) xDir , notchTrail (nHeight ns) yDir
                , notchTrail (nLength ns) _xDir, notchTrail (nHeight ns) _yDir ]

  base = outline <> feet
    where
      outline = centerXY . strokeTrail . mconcat $
                [ notchTrail (nLength ns) xDir, notchTrail (nWidth ns) yDir
                , notchTrail (nLength ns) _xDir, notchTrail (nWidth ns) _yDir ]
      feet = if hasFeet config
             then centerXY $ atPoints (mkP2 <$> [0, x] <*> [0,y]) (repeat mortise)
             else mempty
      mortise = square $ thickness config + clearance config
      y = footLength / 2
      x = (length config - 4 * thickness config)
  top = centerXY $ rect (length config) (width config)
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
      t = thickness config
      x = footLength / 4 - t / 2
  footLength = width config - 3 * thickness config

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

data Joint = Joint
             { forward :: Double
             , depth   :: Double
             , count   :: Int
             , jRelief :: Double
             } deriving (Show, Read)

notchPair :: (TrailLike t, Vn t ~ V2 Double, Transformable t, Monoid t) =>
             Joint -> Direction V2 Double -> t
notchPair joint@(Joint {forward, depth, jRelief }) dir =
   T.trace (show joint) mconcat $
   [ fromOffsets [ forward *^ u
                 , (depth - chordLength) *^ rotateBy 0.25 u]
   , cornerRelief
   , fromOffsets [ (forward - 2 * chordLength) *^ u ]
   , rotateBy 0.75 cornerRelief
   , fromOffsets [ (depth - chordLength) *^ rotateBy 0.75 u ]
   ] where
     u = fromDirection dir
         -- The relief cut is a semicircle; at the (ideal)
         -- inner corner of the joint, the radius bisects
         -- the right angle.
     r = jRelief / 2
     chordLength = if jRelief > 0 then r * sqrt 2 else 0
     -- jog = (jRelief * 0.5 * (sqrt 2 - 1)) *^ rotateBy 0.375 u
     cornerRelief = if jRelief > 0
                    then scale r $ arc (rotateBy 0.625 dir) (-0.5 @@ turn)
                    else mempty
     --                then [ T.trace (show jog) jog, rotateBy 0.5 jog ]
     --                else []

-- | Direction is not part of Joint because the direction is in 2D,
-- according to how the panel is drawn, not the 3D orientation of the
-- assembled box.
notchTrail :: (TrailLike t, Vn t ~ V2 Double, Transformable t, Monoid t) =>
              Joint -> Direction V2 Double -> t
notchTrail joint dir = mconcat $ replicate (count joint) (notchPair joint dir)

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
