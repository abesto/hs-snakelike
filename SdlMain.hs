{-# LANGUAGE ForeignFunctionInterface #-}

module SdlMain where

import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG
import qualified Graphics.UI.SDL as SDL

import qualified UI.Game as UIG
import qualified UI.Menu as Menu
import qualified Model as M

import Control.Lens

-- Startup
foreign export ccall sdl_main :: IO ()
sdl_main :: IO ()
sdl_main = SDL.withInit [SDL.InitEverything] $ do
  game <- M.newGame 15 15
  screen <- SDL.setVideoMode (fromIntegral $ UIG.windowWidth' (game^.M.columns) 16)
                             (fromIntegral $ UIG.windowHeight' (game^.M.rows) 16)
                             32 [SDL.DoubleBuf]
  surface <- SDL.getVideoSurface
  TTFG.init
  font <- TTF.openFont "art/DejaVuSansMono.ttf" 13
  SDL.setCaption "Snake!" "snake"

  Menu.init surface font
  --UIG.init game surface font 15
