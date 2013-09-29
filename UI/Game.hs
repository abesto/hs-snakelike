module UI.Game where

import qualified Data.Int as DI
import qualified Data.IORef as DIORef
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Timer as CCT
import qualified Control.Concurrent.Suspend.Lifted as CCSL
import Control.Lens

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
windowWidth ui@UI{uiGame=g} = windowWidth' (fromIntegral $ g^.M.columns)
                                           (fromIntegral $ uiCellSize ui)

windowWidth' :: Int -> Int -> DI.Int16
windowWidth' columns cellSize = fromIntegral $ columns * cellSize

windowHeight :: UI -> DI.Int16
windowHeight ui@UI{uiGame=g} = windowHeight' (fromIntegral $ g^.M.rows)
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
cellCenter (Cell (CPosition tlx tly) (CPosition trx _) (CPosition _ bly) _) =
  CPosition ((tlx + trx) `div` 2)
            ((tly + bly) `div` 2)

snakePixel :: SDL.Pixel
snakePixel = SDL.Pixel 0x00FF00FF

snakeEyePixel :: SDL.Pixel
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
drawStars ui@UI{uiSurface=surface, uiFont=font, uiGame=game} = mapM_ drawStar $ game^.M.stars
  where
    drawStar (M.SimpleStar pos) = primitiveDraw pos 0xFFFF00FF
    drawStar (M.TimedStar pos _) = primitiveDraw pos 0xFF8800FF
    primitiveDraw position color = do
      let rect = fromCell $ fromPosition ui position
      SDLp.box surface rect (SDL.Pixel color)

writeStatus :: UI -> IO Bool
writeStatus ui = do
  text <- TTF.renderTextSolid (uiFont ui)
    ("Score: " ++ (show $ (uiGame ui)^.M.score) ++ "  " ++
     "Speed: " ++ (show $ uiSpeed ui) ++ " (j/k)")
    (SDL.Color 255 255 255)
  SDL.blitSurface text Nothing (uiSurface ui) Nothing

draw :: UI -> IO ()
draw ui@UI{uiGame=game, uiSurface=surface} = do
  -- Clear the screen. Ugly hack, want to refactor to update instead of redraw everything
  SDLp.box surface (SDL.Rect 0 0 (fromIntegral $ windowWidth ui) (fromIntegral statusBarHeight)) $ SDL.Pixel 0x000000FF
  SDLp.box surface (SDL.Rect 0 (fromIntegral statusBarHeight) (fromIntegral $ windowWidth ui) (statusBarHeight + (fromIntegral $ windowWidth ui))) $ SDL.Pixel 0x000077FF
  -- Write status
  writeStatus ui
  -- Draw snake
  drawSnake ui $ game^.M.snake
  drawSnakeFace ui $ head $ game^.M.snake
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

---- Handle user input ----
handleInput :: DIORef.IORef UI -> IO ()
handleInput uiRef = do
  event <- SDL.waitEvent
  ui <- DIORef.readIORef uiRef
  case event of
    SDL.KeyDown k -> if SDL.symKey k `elem` [SDL.SDLK_UP, SDL.SDLK_RIGHT, SDL.SDLK_DOWN, SDL.SDLK_LEFT]
      then do
        let ui' = ui{uiGame = transform (uiGame ui) (SDL.symKey k)}
        DIORef.writeIORef uiRef ui'
        handleInput uiRef
      else case SDL.symKey k of
        SDL.SDLK_q -> DIORef.writeIORef uiRef ui{uiGame = (uiGame ui)&M.status.~M.Quit}
        SDL.SDLK_k -> modifySpeed ui 1
        SDL.SDLK_j -> modifySpeed ui (-1)
        _ -> handleInput uiRef
    _ -> handleInput uiRef
  where toHeading SDL.SDLK_UP = M.North
        toHeading SDL.SDLK_RIGHT = M.East
        toHeading SDL.SDLK_DOWN = M.South
        toHeading SDL.SDLK_LEFT = M.West
        transform g key = g&M.inputQueue %~ (++[toHeading key])
        modifySpeed ui delta = do
          let ui' = ui{uiSpeed = max 1 (min 10 $ uiSpeed ui + delta)}
          DIORef.writeIORef uiRef ui'
          writeStatus ui'
          handleInput uiRef

tick :: DIORef.IORef UI -> IO Bool
tick uiRef = do
  ui <- DIORef.readIORef uiRef
  game' <- M.stretchIfStarHit 1 $ M.wrapAround $ M.detectCrash $ M.moveSnake $ M.turnSnake $ uiGame ui
  let ui' = ui{uiGame = game'}
  draw ui'
  print game'
  DIORef.writeIORef uiRef ui'
  return True

timedStarTick :: DIORef.IORef UI -> IO ()
timedStarTick uiRef = do
  ui <- DIORef.readIORef uiRef
  let game = uiGame ui
  let ui' = ui{uiGame = M.tickTimedStars game}
  DIORef.writeIORef uiRef ui'
  drawStars ui'
  return ()

gameLoop :: DIORef.IORef UI -> CC.ThreadId -> IO ()
gameLoop uiRef handlerThread = do
  ui <- DIORef.readIORef uiRef
  if (uiGame ui)^.M.status == M.Playing
    then do
      CC.threadDelay $ 1200000 `div` uiSpeed ui
      tick uiRef
      gameLoop uiRef handlerThread
    else do
      CC.killThread handlerThread

waitForQ = do
  event <- SDL.waitEvent
  case event of
    SDL.KeyDown k -> if SDL.symKey k == SDL.SDLK_q
                       then return ()
                       else waitForQ
    _ -> waitForQ

-- I want to play a game.
init game surface font cellSize = do
  uiRef <- DIORef.newIORef UI {
    uiGame = game
  , uiSurface = surface
  , uiCellSize = 16
  , uiFont = font
  , uiSpeed = 5
  }
  handlerThread <- CC.forkIO (handleInput uiRef)
  timedStarTimer <- CCT.repeatedTimer (timedStarTick uiRef) (CCSL.msDelay 1000)
  gameLoop uiRef handlerThread
  CCT.stopTimer timedStarTimer
  ui <- DIORef.readIORef uiRef
  gameOver ui
  waitForQ
  SDLp.box surface (SDL.Rect 0 0 (fromIntegral $ windowWidth ui) (fromIntegral $ windowHeight ui)) $ SDL.Pixel 0x000000FF
