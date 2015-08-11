module XMobar (XMobar.xmobar) where

-- Temporary solution. I think I will migrate to dzen asap

import System.IO (Handle, hPutStrLn)
import XMonad (X)
import XMonad.Util.Run (spawnPipe, unsafeSpawn)
import XMonad.Hooks.DynamicLog (
    PP(..)
  , xmobarColor
  , shorten
  , xmobarPP
  , dynamicLogWithPP
  , xmobarStrip)

myUrgencyHintFgColor :: String
myUrgencyHintFgColor = "#333333"

myUrgencyHintBgColor :: String
myUrgencyHintBgColor = "#F18C96"

-- Xmobar pretty printer. Color scheme zenburn
zenburnPP :: Handle -> PP
zenburnPP xmproc = xmobarPP {
      ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "#DCA3A3" "" . shorten 70
    , ppCurrent = xmobarColor "#CEFFAC" ""
    , ppSep = " | "
    , ppUrgent = xmobarColor myUrgencyHintFgColor myUrgencyHintBgColor . xmobarStrip
  }

xmobar :: IO (X ())
xmobar = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar"
  unsafeSpawn "~/bin/tray"
  return $ dynamicLogWithPP $ zenburnPP xmproc
