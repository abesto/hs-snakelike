{-# LANGUAGE NamedFieldPuns #-}

module UI.Menu where

import qualified Control.Monad.State as CMS
import qualified Data.Int as DI
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import qualified Graphics.UI.SDL.TTF as TTF

import qualified Model as M
import qualified UI.Game as UIG

type MenuAction = Menu -> IO ()

data MenuItem = MenuItem { menuItemIndex  :: Int
                         , menuItemLabel  :: String
                         , menuItemAction :: MenuAction
                         }

data Menu = Menu { menuSurface      :: SDL.Surface
                 , menuFont         :: TTF.Font
                 , menuCurrentItem  :: Int
                 , menuItems        :: [MenuItem]
                 }

textColor = SDL.Color 0 255 0
cursorPixel = SDL.Pixel 0x00FF00FF
backgroundPixel = SDL.Pixel 0x000000FF

menuItemRect :: Int -> SDL.Rect
menuItemRect index = SDL.Rect marginLeft top marginLeft height
                     where marginLeft = 30
                           top = marginTop + index * height
                           marginTop = 40
                           height = 20
                           
drawMenuItems menu = mapM_ (drawMenuItem menu) (menuItems menu)
                     where drawMenuItem Menu{menuSurface, menuFont} MenuItem{menuItemIndex, menuItemLabel} = do
                             renderedText <- TTF.renderTextSolid menuFont menuItemLabel textColor
                             SDL.blitSurface renderedText Nothing menuSurface $ Just $ menuItemRect menuItemIndex

drawCursorWithPixel :: SDL.Pixel -> Menu -> IO ()
drawCursorWithPixel pixel Menu{menuSurface, menuCurrentItem} = do
  SDLp.filledTrigon menuSurface x0 y0 x1 y1 x2 y2 pixel
  return ()
  where x0 = fromIntegral $ SDL.rectX rect - 15 :: DI.Int16
        y0 = fromIntegral $ SDL.rectY rect + 3  :: DI.Int16
        y2 = y0 + (fromIntegral $ SDL.rectH rect) - 8
        y1 = (y0 + y2) `div` 2
        x1 = x0 + 10
        x2 = x0
        rect = menuItemRect menuCurrentItem

drawCursor = drawCursorWithPixel cursorPixel
clearCursor = drawCursorWithPixel backgroundPixel

handleMenu :: Menu -> IO ()
handleMenu menu@Menu{menuSurface, menuCurrentItem, menuItems} = do
  SDL.flip menuSurface
  event <- SDL.waitEventBlocking
  case event of
    SDL.KeyDown k -> case SDL.symKey k of
      SDL.SDLK_DOWN   -> moveCursor 1
      SDL.SDLK_UP     -> moveCursor (-1)
      SDL.SDLK_RETURN -> (menuItemAction $ menuItems !! menuCurrentItem) menu
      _  -> handleMenu menu
    _ -> handleMenu menu
    where moveCursor delta = if (menuCurrentItem + delta >= length menuItems) || (menuCurrentItem + delta < 0)
                                then handleMenu menu
                                else do
                                  clearCursor menu
                                  let menu' = menu{menuCurrentItem = menuCurrentItem + delta}
                                  drawCursor menu'
                                  handleMenu menu'

quit :: MenuAction
quit _ = return ()

newGame :: MenuAction
newGame menu = do
  game <- M.newGame 15 15
  UIG.init game (menuSurface menu) (menuFont menu) 15
  initMenu menu

initMenu :: Menu -> IO ()
initMenu menu = do
  drawMenuItems menu
  drawCursor menu
  handleMenu menu

init :: SDL.Surface -> TTF.Font -> IO ()
init surface font = initMenu $  Menu surface font 0 $ buildMenuItems [ ("New game", newGame)
                                                                     , ("Quit", quit)
                                                                     ]
  where buildMenuItems = zipWith buildItem [0..]
                 where buildItem index (label, action) = MenuItem index label action

