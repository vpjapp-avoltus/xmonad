module Prompt (xdg_open, browser) where

import qualified XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt
import XMonad (X)

launchApp ::  AL.Application -> X ()
launchApp = AL.launchApp defaultXPConfig

xdg_open :: X ()
xdg_open = launchApp "xdg-open"

browser :: X ()
browser = launchApp "x-www-browser"
