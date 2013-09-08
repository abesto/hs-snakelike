{-# LANGUAGE NamedFieldPuns #-}

module UI.Menu where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Error

import qualified Control.Monad.State as CMS
import qualified Data.Int as DI
import qualified Data.Map as Map
import qualified Data.Maybe as DM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLp
import qualified Graphics.UI.SDL.TTF as TTF

import qualified Model as M
import qualified UI.Game as UIG
import qualified UI.Keys as UIK

type MenuAction = Menu -> ErrorT String IO Menu

data MenuItem = MenuItem { menuItemIndex  :: Int
                         , menuItemLabel  :: String
                         , menuItemAction :: MenuAction
                         }

data Menu = Menu { menuSurface      :: SDL.Surface
                 , menuFont         :: TTF.Font
                 , menuCurrentItem  :: Int
                 , menuItems        :: [MenuItem]
                 , menuKeys         :: MenuKeys
                 }

data MenuKeys = MenuKeys { menuKeyUp     :: UIK.KeyConf
                         , menuKeyDown   :: UIK.KeyConf
                         , menuKeyAccept :: UIK.KeyConf }

errQuit = "QUIT"

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

handleMenu :: Menu -> ErrorT String IO ()
handleMenu menu@Menu{menuKeys, menuSurface, menuCurrentItem, menuItems} = do
  lift $ drawCursor menu
  lift $ SDL.flip menuSurface
  event <- lift $ SDL.waitEventBlocking 
  menu' <- UIK.handleEvent event $ Map.fromList 
                [ (menuKeyDown menuKeys,   moveCursor 1)
                , (menuKeyUp menuKeys,     moveCursor (-1))
                , (menuKeyAccept menuKeys, (menuItemAction $ menuItems !! menuCurrentItem) menu)
                , (UIK.Otherwise,          return menu)
                ]
  handleMenu menu'
  where moveCursor delta = lift $ if (menuCurrentItem + delta >= length menuItems) || (menuCurrentItem + delta < 0)
                                  then return menu
                                  else clearCursor menu >> return menu{menuCurrentItem = menuCurrentItem + delta}

quit :: MenuAction
quit _ = throwError errQuit

newGame :: MenuAction
newGame menu = lift $ do
  game <- M.newGame 15 15
  UIG.init game (menuSurface menu) (menuFont menu) 15
  drawMenuItems menu
  return menu

init :: SDL.Surface -> TTF.Font -> IO ()
init surface font = do
  let i = buildMenuItems [ ("New game", newGame)
                         , ("Quit", quit)
                         ]
  let k = MenuKeys { menuKeyUp = DM.fromJust $ UIK.readKeyConf "UP"
                   , menuKeyDown = DM.fromJust $ UIK.readKeyConf "DOWN"
                   , menuKeyAccept = DM.fromJust $ UIK.readKeyConf "RETURN"
                   }
  let menu = Menu surface font 0 i k
  drawMenuItems menu
  runErrorT $ handleMenu menu
  return ()
  where buildMenuItems = zipWith buildItem [0..]
                 where buildItem index (label, action) = MenuItem index label action

