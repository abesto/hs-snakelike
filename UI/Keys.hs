{-# LANGUAGE TemplateHaskell #-}

module UI.Keys where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Error

import qualified Data.Text as DT
import qualified Data.Map as DM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keysym as SDLK
import qualified GenerateRead as GR

$(GR.mkRead ''SDLK.SDLKey)
$(GR.mkRead ''SDLK.Modifier)

errNoHandler = "NO_HANDLER"

data KeyConf = Otherwise | KeyConf { keyConfKey :: SDLK.SDLKey
                                   , keyConfModifiers :: [SDLK.Modifier]
                                   } deriving (Eq, Ord)

readKeyConf :: String -> Maybe KeyConf
readKeyConf s = do
    let parts = map DT.unpack $ DT.splitOn (DT.singleton '+') (DT.pack s)
    key <- readSDLKey $ "SDLK_" ++ last parts
    modifiers <- mapM (readModifier . ("KeyMod" ++)) $ init parts
    return KeyConf { keyConfKey = key
                   , keyConfModifiers = modifiers
                   }

fromKeysym :: SDLK.Keysym -> KeyConf
fromKeysym (SDLK.Keysym key modifiers _) = KeyConf key modifiers

handleEvent :: SDL.Event -> DM.Map KeyConf (ErrorT String IO a) -> ErrorT String IO a
handleEvent (SDL.KeyDown keysym) keymap = handleKey (fromKeysym keysym) keymap
handleEvent _ keymap = handleKey Otherwise keymap

handleKey :: KeyConf -> DM.Map KeyConf (ErrorT String IO a) -> ErrorT String IO a
handleKey keyconf keymap = case DM.lookup keyconf keymap of
    Nothing     -> if keyconf == Otherwise then throwError "No handler specified" else handleKey Otherwise keymap
    Just action -> action
