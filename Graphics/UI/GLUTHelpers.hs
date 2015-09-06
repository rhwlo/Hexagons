module Graphics.UI.GLUTHelpers (
  ColoredPolygonT,
  GL3T,
  drawPolygonFT,
  drawScaledPolygonFT,
  drawVertexFT,
  setColorFT
) where

import Control.Applicative ((<$>))
import Graphics.UI.GLUT

type GL3T = (GLfloat, GLfloat, GLfloat) -- a 3-tuple of GLfloats
type ColoredPolygonT = (GL3T, [GL3T])   -- a 3-tuple for color, and a list of 3-tuples for vertices.

drawPolygonFT :: ColoredPolygonT            -- a polygon tuple to draw
              -> IO ()                      -- the rendered polygon
drawPolygonFT (colorT, vertices) = renderPrimitive Polygon $ setColorFT colorT >>
                                                             mapM_ drawVertexFT vertices

drawScaledPolygonFT :: Size                 -- the size to scale to
                    -> ColoredPolygonT      -- a polygon tuple to draw scaled
                    -> IO ()                -- the rendered polygon
drawScaledPolygonFT size polygonT = drawPolygonFT $ (map (scaleVertexT size)) <$> polygonT

drawVertexFT :: GL3T                        -- a 3-tuple of GLfloats describing a Vertex3
             -> IO ()                       -- the rendered vertex (in context)
drawVertexFT = vertex . (uncurry3 Vertex3)

scaleVertexT :: Size                    -- the size to scale to
             -> GL3T                    -- a 3-tuple of (x, y, z)
             -> GL3T                    -- a scaled 3-tuple of (x, y, z)
scaleVertexT (Size xSize ySize) (x, y, z) = let
  scaledX = 2 * x / (fromIntegral xSize)
  scaledY = 2 * y / (fromIntegral ySize)
  in (scaledX, scaledY, z)

setColorFT :: GL3T                         -- a 3-tuple of GLfloats describing an RGB color
           -> IO ()                        -- the applied color (in context)
setColorFT = color . (uncurry3 Color3)

uncurry3 :: (a -> b -> c -> d)          -- a function of three arguments
         -> (a, b, c)                   -- a 3-tuple of those arguments
         -> d                           -- the function's result
uncurry3 f (a, b, c) = f a b c
