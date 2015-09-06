import Graphics.UI.GLUT
import Graphics.UI.GLUTHelpers (drawScaledPolygonFT)
import Hexagons (fillWithHexagons)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _window <- createWindow "Imagine an infinite plane of hexagons"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    size <- get windowSize
    mapM_ (drawScaledPolygonFT size) $ fillWithHexagons size 30
    flush
