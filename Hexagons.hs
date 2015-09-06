module Hexagons where

import Control.Applicative ((<$>))
import Data.List (foldl')
import Data.Monoid (Monoid, mappend, mconcat, mempty, (<>))
import Graphics.UI.GLUT
import Graphics.UI.GLUTHelpers

class GL3Table g where
  toGL3T :: g -> GL3T

data HexCoordinate = HexC GLfloat GLfloat deriving (Show, Eq)
instance Monoid HexCoordinate where
  mempty = HexC 0 0
  mappend (HexC x y) (HexC x' y') = HexC (x + x') (y + y')
  mconcat = foldl' mappend mempty
instance GL3Table HexCoordinate where
  toGL3T = toGL3T . (hexToCartesian 1)

data CartCoordinate = CartC GLfloat GLfloat deriving (Show, Eq)
instance Monoid CartCoordinate where
  mempty = CartC 0 0
  mappend (CartC x y) (CartC x' y') = CartC (x + x') (y + y')
  mconcat = foldl' mappend mempty
instance GL3Table CartCoordinate where
  toGL3T (CartC x y) = (x, y, 0)

fillWithHexagons :: Size                        -- the size to fill with hexagons
                 -> GLfloat                     -- the radius of hexagons to use
                 -> [ColoredPolygonT]           -- a list of polygons to pass to drawScaledPolygonFT
fillWithHexagons (Size width height) radius = getPolygonTuples <$> hexCoordinateList
  where
    yPackingRadius :: GLfloat
    yPackingRadius = radius * sqrt(3) / 2
    xPackingRadius :: GLfloat
    xPackingRadius = radius * 3/4
    yOriginMax = ((fromIntegral height) - yPackingRadius) / (2 * yPackingRadius)
    ySize = yOriginMax * 2
    xMax = ((fromIntegral width) - xPackingRadius) / (2 * xPackingRadius)
    xRange :: [Int]
    xRange = [(negate (ceiling xMax))..(ceiling xMax)]
    hexCoordinateList :: [HexCoordinate]
    hexCoordinateList = xRange >>= \x -> let
        yMin = negate (ceiling yOriginMax + (div x 2) + (mod x 2))
        yMax = yMin + ceiling ySize + (mod x 2)
        yRange = [yMin..yMax]
      in [HexC (fromIntegral x) (fromIntegral y) | y <- yRange ]
    getPolygonTuples :: HexCoordinate            -- the (hexX, hexY) coordinates at which to draw a hex
                     -> ColoredPolygonT          -- the ColoredPolygonT of the hexagon
    getPolygonTuples hexCoordinate  = let
        (HexC hexX hexY) = hexCoordinate
      in
        (phiSinebow (floor (hexX + 3 * hexY)), hexagonVerticesFor hexCoordinate radius)

hexagonVerticesFor :: HexCoordinate   -- a HexCoordinate for the position of the center
                   -> GLfloat         -- the radius of the hexagon
                   -> [GL3T]          -- the list of 3-tuple (x, y, z) coordinates for Vertex3
hexagonVerticesFor hexCenter radius = toGL3T <$> (((<>) cartCenter) <$> unitHexagon)
  where
    cartCenter = hexToCartesian radius hexCenter
    unitHexagon :: [CartCoordinate]
    unitHexagon = [CartC (radius * (cos phi)) (radius * (sin phi)) | phi <- [0,(pi/3)..(5*pi/3)]]

hexToCartesian :: GLfloat                   -- the unit-radius for a hexagon
               -> HexCoordinate             -- the hexagonal coordinate to convert
               -> CartCoordinate            -- the cartesian coordinate resulting
hexToCartesian radius (HexC hexX hexY) = let
    shortRadius = radius * 3 ** 0.5 / 2
    y :: GLfloat
    y = shortRadius * (2 * hexY + hexX)
    x :: GLfloat
    x = 3 / 2 * radius * hexX
  in CartC x y

sinebow :: GLfloat      -- a float to convert to a sinebow color
        -> GL3T         -- a 3-tuple of color. See http://basecase.org/env/on-rainbows
sinebow n = (red, green, blue)
  where
    scaling = pi
    red = (sin (n * scaling)) ** 2
    green = (sin ((n + 1 / 3) * scaling)) ** 2
    blue = (sin ((n + 2 / 3) * scaling)) ** 2

phiSinebow :: Integer           -- an integer to choose the nth color from the sinebow
           -> GL3T              -- the nth color from the sinebow. See same as `sinebow`
phiSinebow n = let phi = 1 + 2 ** 0.5
  in sinebow (fromIntegral n * phi)
