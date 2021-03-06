import XMonad
import Data.Map (Map)
import qualified Data.Map as M
import XMonad.Actions.CycleWS (swapNextScreen)
import qualified XMonad.StackSet as W
import XMonad.Layout.Tabbed (TabbedDecoration, Theme(..), defaultTheme, tabbed, simpleTabbed, shrinkText)
import XMonad.Layout.PerWorkspace (onWorkspace, PerWorkspace)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Accordion
import XMonad.Layout.Master
import XMonad.Layout.MagicFocus
import XMonad.Layout.DwmStyle
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Prompt (defaultXPConfig)
import Screen (nextScreen, prevScreen)
import TopicSpace
import XMobar
import Prompt
import System.Exit (exitWith, ExitCode(..))
import XMonad.Hooks.EwmhDesktops

-- Search engines inside submaps
searchSubmaps = submap . M.fromList $
  [ ((0, xK_g), promptSearch defaultXPConfig google)
  , ((0, xK_d), promptSearch defaultXPConfig (searchEngine "duckduckgo" "http://duckduckgo.com/?q="))
  , ((0, xK_a), promptSearch defaultXPConfig alpha) -- Wolfram alpha
  , ((0, xK_w), promptSearch defaultXPConfig wikipedia)
  , ((0, xK_j), promptSearch defaultXPConfig (searchEngine "java7api" "https://encrypted.google.com/search?hl=en&q=java+7+api+"))]


-- Layout
myTabConfig ::  Theme
myTabConfig = defaultTheme {
      activeBorderColor = "#DCDCCC"
    , activeTextColor = "#DCDCCC"
    , activeColor = "#3F3F3F"
    , fontName = "xft:Inconsolata-9"
    , inactiveBorderColor = "#262626"
    , inactiveTextColor = "#9FAFAF"
    , inactiveColor = "#262626"
  }

myLayout = onWorkspace "web" webLayout $
           onWorkspace "pdf" pdfLayout $
           onWorkspace "documents" documentLayout $
           defLayout
  where
    -- Default layout
    defLayout = avoidStruts (tiled ||| Mirror tiled ||| tabLayout ||| Full)
    -- Pdfs are restricted to tabs
    pdfLayout =  magicFocus readLayout ||| tiled ||| avoidStruts tabLayout
    readLayout = dwmStyle shrinkText myTabConfig (mastered (1/100) (2/3) Accordion)
    -- Documents are by default tabs, but have looser restrictions
    documentLayout = avoidStruts (tabLayout ||| Full ||| tiled ||| Mirror tiled)
    -- Web is either tabbed, full, or tiled
    webLayout = avoidStruts (readLayout ||| tabLayout ||| Full ||| tiled)
    tiled = avoidStruts $ Tall nmaster delta ratio
    -- I need to restrict the type or type inferencer can't deduce type classes
    tabLayout :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
    tabLayout = tabbed shrinkText myTabConfig
    delta = 3/100
    ratio = 1/2
    nmaster = 1

-- Manage hooks
-- Move programs to their workspaces
myManageHook = composeAll $ concat [
      webHooks
    , pdfHooks
    , documentHooks
    , floatHooks
    , imHooks
    , vmHooks
  ]
  where
    hook y = map (\x -> className =? x --> y)
    vmHooks = hook (doShift "citrix") [
      "PuTTY Configuration"
      , "Wfica_Seamless"
      ]
    webHooks = hook (doShift "web") [
          "Firefox"
        , "Midori"
        , "Chromium-browser"
        , "Uzbl-tabbed"
        , "Uzbl-core"
      ]
    pdfHooks = hook (doShift "pdf") [
          "Evince"
        , "Okular"
        , "Kpdf"
        , "Xdvi"
      ]
    documentHooks = hook (doShift "documents") [
          "Openoffice.org 3.2" -- really? 3.2? Could it be any more specific :/
        , "libreoffice-calc"
        , "Assistant"
        , "Bouml"
      ]
    floatHooks = hook doFloat [
          "SMplayer"
        , "Gimp"
        , "MPlayer"
        , "Kaffeine"
        , "Hamster-time-tracker"
        , "Hamster"
        , "Time-Tracker"
      ]
    imHooks = hook (doShift "1:im") [
          "hotot"
        , "Kopete"
      ]

-- Key layout
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask})= M.unions [
      M.fromList $ [
          -- Exit
          ((shiftMask .|. modMask, xK_q), io (exitWith ExitSuccess))
          -- Start terminal
        , ((shiftMask .|. modMask, xK_Return), spawn $ XMonad.terminal conf)

          -- Choose next window
        , ((modMask, xK_j), windows W.focusDown)
        , ((modMask, xK_Tab), windows W.focusDown)

          -- Choose previous window
        , ((modMask, xK_k), windows W.focusUp)
        , ((shiftMask .|. modMask, xK_Tab), windows W.focusUp)

          -- Move window down
        , ((shiftMask .|. modMask, xK_j), windows W.swapDown)
          -- Move window up
        , ((shiftMask .|. modMask, xK_k), windows W.swapUp)

          -- Shrink the master area
        , ((modMask, xK_h), sendMessage Shrink)

          -- Expand the master area
        , ((modMask, xK_l), sendMessage Expand)

        -- Swap master and focused
        , ((shiftMask .|. modMask, xK_m), windows W.swapMaster)

        -- Lock the screen
        , ((controlMask .|. modMask, xK_l), spawn "gnome-screensaver-command --lock; /home/vpjapp/bin/spotify.sh pause")

        -- Pause spotify
        , ((shiftMask .|. modMask, xK_s), spawn "/home/vpjapp/bin/spotify.sh playpause")

          -- Spawn xinerama screens
        , ((shiftMask .|. modMask, xK_space), swapNextScreen)

          -- Go to left screen
        , ((shiftMask .|. modMask, xK_Left), nextScreen)
          -- Go to right screen
        , ((shiftMask .|. modMask, xK_Right), prevScreen)

         -- Go to left screen
        --, ((shiftMask .|. modMask, xK_h), prevScreen)
          -- Go to right screen
        --, ((shiftMask .|. modMask, xK_l), nextScreen)

         -- Close window
        , ((shiftMask .|. modMask, xK_c), kill)

          -- Rotate layout algorithms
        , ((modMask, xK_space), sendMessage NextLayout)

          -- DMenu
        , ((modMask, xK_p), spawn "/usr/bin/dmenu_run")

          -- Prompt
        , ((shiftMask .|. modMask, xK_e), xdg_open)

          -- Search submaps
        , ((modMask, xK_s), searchSubmaps)

          -- Hamster window
        , ((modMask, xK_t), spawn "hamster-time-tracker")

          -- Unfloat
        ,((modMask,  xK_z), withFocused $ windows . W.sink)

      ]
    , topicKeys conf
  ]

main :: IO ()
main = do
  loghook <- xmobar
  xmonad $ ewmh defaultConfig {
        modMask = mod1Mask -- Alt
      , terminal = "/usr/bin/terminator"
      , clickJustFocuses = False
      , keys = myKeys
      , workspaces = myTopics
--      , logHook = (loghook >> updatePointer (Relative 0.5 0.5))
      , logHook = loghook 
      , layoutHook = smartBorders $ myLayout
      , startupHook = setWMName "LG3D"
      , borderWidth = 2
      , normalBorderColor = "#444444"
      , focusedBorderColor = "#7F9F7F"
      , manageHook = manageDocks <+> myManageHook
      , focusFollowsMouse = False
      , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
    } `additionalKeysP` [("<XF86Sleep>", spawn "sudo pm-suspend")]
