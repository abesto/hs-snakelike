{-# LANGUAGE DoAndIfThenElse, ForeignFunctionInterface #-}

-- 17:20


module SdlMain where

import qualified Data.Char as DC
import qualified Data.IORef as DIORef
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Timer as CCT
import qualified Control.Concurrent.Suspend.Lifted as CCSL
import qualified Graphics.UI.SDL as SDL

import qualified Model as M
import qualified Display as D

---- Handle user input ----
handleInput :: DIORef.IORef D.UI -> IO ()
handleInput uiRef = do
  event <- SDL.waitEvent
  ui <- DIORef.readIORef uiRef
  case event of
    SDL.KeyDown k -> if SDL.symKey k `elem` [SDL.SDLK_UP, SDL.SDLK_RIGHT, SDL.SDLK_DOWN, SDL.SDLK_LEFT] 
      then do
        let ui' = ui{D.uiGame = transform (D.uiGame ui) (SDL.symKey k)}
        DIORef.writeIORef uiRef ui'
        handleInput uiRef
      else case SDL.symKey k of
        SDL.SDLK_q -> DIORef.writeIORef uiRef ui{D.uiGame = (D.uiGame ui){M.gameStatus=M.Quit}}
        SDL.SDLK_k -> modifySpeed ui 1
        SDL.SDLK_j -> modifySpeed ui (-1)
        _ -> handleInput uiRef
    _ -> handleInput uiRef  
  where toHeading SDL.SDLK_UP = M.North
        toHeading SDL.SDLK_RIGHT = M.East
        toHeading SDL.SDLK_DOWN = M.South
        toHeading SDL.SDLK_LEFT = M.West
        transform game@M.Game{M.gameInputQueue=q} key = game{M.gameInputQueue=q ++ [toHeading key]}
        modifySpeed ui delta = do
          let ui' = ui{D.uiSpeed = max 1 (min 10 $ D.uiSpeed ui + delta)}
          DIORef.writeIORef uiRef ui'
          D.writeStatus ui'
          handleInput uiRef

tick :: DIORef.IORef D.UI -> IO Bool
tick uiRef = do
  ui <- DIORef.readIORef uiRef
  game' <- M.stretchIfStarHit 1 $ M.wrapAround $ M.detectCrash $ M.moveSnake $ M.turnSnake $ D.uiGame ui
  let ui' = ui{D.uiGame = game'}
  D.draw ui'
  DIORef.writeIORef uiRef ui'
  return True

timedStarTick :: DIORef.IORef D.UI -> IO ()
timedStarTick uiRef = do
  ui <- DIORef.readIORef uiRef
  let game = D.uiGame ui
  let ui' = ui{D.uiGame = M.tickTimedStars game}
  DIORef.writeIORef uiRef ui'
  D.drawStars ui'
  return ()

gameLoop :: DIORef.IORef D.UI -> CC.ThreadId -> IO ()
gameLoop uiRef handlerThread = do
  ui <- DIORef.readIORef uiRef
  if (M.gameStatus $ D.uiGame ui) == M.Playing 
    then do
      CC.threadDelay $ 1200000 `div` D.uiSpeed ui
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

-- Startup
foreign export ccall sdl_main :: IO ()
sdl_main :: IO ()
sdl_main = SDL.withInit [SDL.InitEverything] $ do
  game <- M.newGame 15 15
  uiRef <- D.initUI game 16
  handlerThread <- CC.forkIO (handleInput uiRef)
  timedStarTimer <- CCT.repeatedTimer (timedStarTick uiRef) (CCSL.msDelay 1000)
  gameLoop uiRef handlerThread
  CCT.stopTimer timedStarTimer

  ui <- DIORef.readIORef uiRef
  D.gameOver ui
  waitForQ
