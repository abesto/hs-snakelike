module Display where

import qualified Data.Int as DI
import qualified Data.IORef as DIORef
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

import qualified Model as M

data CPosition = CPosition {cx :: DI.Int16, cy :: DI.Int16 }
data Cell = Cell { tl :: CPosition, tr :: CPosition, bl :: CPosition, br :: CPosition }
data UI = UI {
      uiGame  :: M.Game
    , uiSurface :: SDL.Surface
    , uiCellSize :: Int
    , uiFont :: TTF.Font
    , uiSpeed :: Int
    }

statusBarHeight :: Int
statusBarHeight = 20

windowWidth :: UI -> DI.Int16
windowWidth ui@UI{uiGame=g} = windowWidth' (fromIntegral $ M.gameColumns g) 
                                           (fromIntegral $ uiCellSize ui) 
windowWidth' :: Int -> Int -> DI.Int16
windowWidth' columns cellSize = fromIntegral $ columns * cellSize

windowHeight :: UI -> DI.Int16
windowHeight ui@UI{uiGame=g} = windowHeight' (fromIntegral $ M.gameRows g)
                                             (fromIntegral $ uiCellSize ui)
windowHeight' :: Int -> Int -> DI.Int16                                             
windowHeight' rows cellSize = fromIntegral $ statusBarHeight + rows * cellSize

newCPosition :: Int -> Int -> CPosition
newCPosition x y = CPosition (fromIntegral x) (fromIntegral y)

fromPosition :: UI -> M.Position -> Cell
fromPosition UI{uiCellSize=cellSize} (M.Position x y) = 
  Cell (newCPosition tlx $ tly + fromIntegral statusBarHeight)
       (newCPosition brx $ tly + fromIntegral statusBarHeight)
       (newCPosition tlx $ bry + fromIntegral statusBarHeight)
       (newCPosition brx $ bry + fromIntegral statusBarHeight)
  where tlx = x * cellSize
        tly = y * cellSize
        brx = tlx + cellSize 
        bry = tly + cellSize

cellCenter :: Cell -> CPosition
cellCenter (Cell (CPosition tlx tly) (CPosition trx try) (CPosition blx bly) _) = 
  CPosition ((tlx + trx) `div` 2)
            ((tly + bly) `div` 2)

snakePixel = SDL.Pixel 0x00FF00FF
snakeEyePixel = SDL.Pixel 0xFF0000FF

drawSnake :: UI -> [M.Position] -> IO Bool
drawSnake ui@UI{uiSurface=surface, uiCellSize=cellSize} (current:next:rest) = do
  drawSnakeBody current
  drawConnection current next
  drawConnection next current
  if not $ null rest 
    then drawSnake ui (next:rest) 
    else drawSnakeBody next
  where drawSnakeBody :: M.Position -> IO Bool
        drawSnakeBody p = SDLp.filledEllipse surface (cx center) (cy center) r r snakePixel
          where center = cellCenter $ fromPosition ui p
                r = fromIntegral $ (cellSize `div` 2) - 2
        drawConnection :: M.Position -> M.Position -> IO Bool
        drawConnection p0 p1 = SDLp.box surface (SDL.Rect x0 y0 x1 y1) snakePixel
          where center0 = cellCenter $ fromPosition ui p0
                center1 = cellCenter $ fromPosition ui p1
                heading = M.headingBetween p0 p1
                x0 = fromIntegral $ cx center0 - 1
                y0 = fromIntegral $ cy center0 - 1
                connLength = cellSize `div` 2 + 1
                x1 = x0 + case heading of
                            M.North -> 2
                            M.South -> 2
                            M.East ->  connLength
                            M.West -> -connLength
                y1 = y0 + case heading of
                            M.East -> 2
                            M.West -> 2
                            M.South ->  connLength
                            M.North -> -connLength

drawSnakeFace :: UI -> M.Position -> IO Bool
drawSnakeFace ui@UI{uiSurface=surface} p = do
  SDLp.filledEllipse surface (cx center - 5) y r r snakeEyePixel
  SDLp.filledEllipse surface (cx center + 5) y r r snakeEyePixel
  where center = cellCenter $ fromPosition ui p
        y = cy center - 3
        r = 2

fromCell :: Cell -> SDL.Rect
fromCell cell = 
  SDL.Rect (fromIntegral$cx$tl$cell)
           (fromIntegral$cy$tl$cell)
           (fromIntegral$cx$br$cell)
           (fromIntegral$cy$br$cell)

drawStars :: UI -> IO ()
drawStars ui@UI{uiSurface=surface, uiFont=font, uiGame=M.Game{M.gameStars=stars}} = mapM_ drawStar stars
  where 
    drawStar (M.SimpleStar pos) = primitiveDraw pos 0xFFFF00FF
    drawStar (M.TimedStar pos _) = primitiveDraw pos 0xFF8800FF
    primitiveDraw position color = do
      let rect = fromCell $ fromPosition ui position
      SDLp.box surface rect (SDL.Pixel color)

writeStatus :: UI -> IO Bool
writeStatus ui = do
  text <- TTF.renderTextSolid (uiFont ui) 
    ("Score: " ++ (show $ M.gameScore $ uiGame ui) ++ "  " ++
     "Speed: " ++ (show $ uiSpeed ui) ++ " (j/k)")
    (SDL.Color 255 255 255)
  SDL.blitSurface text Nothing (uiSurface ui) Nothing

draw :: UI -> IO ()
draw ui@UI{uiGame=game@M.Game{M.gameSnake=snake}, uiSurface=surface} = do
  -- Clear the screen. Ugly hack, want to refactor to update instead of redraw everything
  SDLp.box surface (SDL.Rect 0 0 (fromIntegral $ windowWidth ui) (fromIntegral statusBarHeight)) $ SDL.Pixel 0x000000FF
  SDLp.box surface (SDL.Rect 0 (fromIntegral statusBarHeight) (fromIntegral $ windowWidth ui) (statusBarHeight + (fromIntegral $ windowWidth ui))) $ SDL.Pixel 0x000077FF
  -- Write status
  writeStatus ui
  -- Draw snake
  drawSnake ui snake
  drawSnakeFace ui $ head snake
  -- Draw star
  drawStars ui
  SDL.flip surface

gameOver :: UI -> IO ()
gameOver ui@UI{uiSurface=surface} = do
  font <- TTF.openFont "art/DejaVuSansMono.ttf" 30
  text <- TTF.renderTextSolid font "Game Over" (SDL.Color 255 0 0)
  SDL.blitSurface text Nothing surface (Just $ SDL.Rect 40 40 0 0)
  SDL.flip surface
  return ()

initUI :: M.Game -> Int -> IO (DIORef.IORef UI)
initUI game cellSize = do
  screen <- SDL.setVideoMode (fromIntegral $ windowWidth' (M.gameColumns game) 16) 
                             (fromIntegral $ windowHeight' (M.gameRows game) 16) 
                             32 [SDL.DoubleBuf]
  surface <- SDL.getVideoSurface
  TTFG.init
  font <- TTF.openFont "art/DejaVuSansMono.ttf" 13
  SDL.setCaption "Snake!" "snake"
  DIORef.newIORef UI {
    uiGame = game
  , uiSurface = surface
  , uiCellSize = 16
  , uiFont = font
  , uiSpeed = 5
  }

