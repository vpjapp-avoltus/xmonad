module TopicSpace (topicKeys, myTopics) where

--import XMonad.Actions.CycleWS
import XMonad.Actions.TopicSpace
import XMonad
import qualified Data.Map as M
import Data.Map (Map)
import XMonad.Prompt.Workspace
import XMonad.Prompt (defaultXPConfig, XPConfig(..))
import XMonad.Actions.GridSelect
import XMonad.Util.Run (safeSpawn, unsafeSpawn)
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu (dmenu)
import Control.Applicative
import Data.List (nub)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap

myTopics :: [Topic]
myTopics = [
      "irc"
    , "web"
    , "mail"
    , "pdf"
    , "documents"
    , "adhoc"
    , "eclipse"
    , "music"
    , "tale:term"
    , "yle:term"
    , "ess:term"
    , "ksml:term"
    , "term"
 ]

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = safeRunInTerm dir Nothing

safeRunInTerm :: Dir -> Maybe String -> X ()
safeRunInTerm dir Nothing = asks (terminal . config) >>= \t -> safeSpawn t ["-cd", dir]
safeRunInTerm dir (Just command) = asks (terminal . config) >>= \t -> safeSpawn t ["-cd", dir, "-e", command]

unsafeRunInTerm :: Dir -> Maybe String -> X ()
unsafeRunInTerm dir Nothing = asks (terminal . config) >>= \t -> (unsafeSpawn $ t ++ " -cd " ++ dir)
unsafeRunInTerm dir (Just command) = asks (terminal . config) >>= \t -> (unsafeSpawn $ t ++ " -cd " ++ dir ++  " -e " ++ command)

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig {
    topicDirs = M.fromList $ [
        ("documents", "/home/masse/Documents")
      , ("xmonad", "/home/masse/.xmonad")
    ]
  , defaultTopicAction = const spawnShell
  , defaultTopic = "irc"
  , topicActions = M.fromList $ [
        ("web", spawn "x-www-browser")
      , ("mail", spawn "thunderbird")
    ]
  }

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

-- dmenuGoto :: X ()
dmenuGoto = withWindowSet $ \ws -> do
  let topics = map W.tag $ W.hidden ws
  dmenu topics >>= goto

dmenuMove = do
  topics <- (map W.tag . W.hidden) `fmap` gets windowset
  dmenu topics >>= \t -> windows (\ws -> W.shift t ws)

modificationSubmaps = submap . M.fromList $
  [ ((0, xK_a), addWorkspacePrompt defaultXPConfig),
    ((0, xK_d), removeEmptyWorkspace)
    ,((0, xK_v), gridselectWorkspace defaultGSConfig W.greedyView) 
  ]

topicKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
topicKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
      ((modMask, xK_g), dmenuGoto)
    , ((shiftMask .|. modMask, xK_g), dmenuMove)
    , ((controlMask .|. modMask, xK_g), goToSelected defaultGSConfig)
    , ((controlMask .|. modMask .|. shiftMask, xK_y), gridselectWorkspace defaultGSConfig (\ws -> W.greedyView ws . W.shift ws) )
    , ((modMask,  xK_Return), currentTopicAction myTopicConfig)
    , ((modMask, xK_w), modificationSubmaps)
    , ((modMask, xK_Right),  dmenuGoto)
    , ((modMask, xK_Left),   dmenuGoto)
  ]
  ++ [((m .|. modMask, k), windows $ f i) |
      (i, k) <- zip (XMonad.workspaces conf) [xK_F1..xK_F12]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]



