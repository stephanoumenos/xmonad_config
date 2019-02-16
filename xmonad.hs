import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CopyWindow
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen

import System.Exit
import System.Clipboard

import Control.Monad
import Control.Monad.IO.Class

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio

-- pls no bully
myTerminal = "xfce4-terminal --hide-menubar"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth = 4

myModMask = mod1Mask

myWorkspaces = map show [1..10]

myNormalBorderColor  = "dddddd"
myFocusedBorderColor = "#ff69b4"

lowerVolume =  "amixer -c 0 set Headphone 4- ; amixer -c 1 set Headphone 4- ; amixer -c 2 set Headphone 4-"
raiseVolume =  "amixer -c 0 set Headphone 4+ ; amixer -c 1 set Headphone 4+ ; amixer -c 2 set Headphone 4+"

myScratchPads = [ NS ("terminal"++n) (myTerminal ++ " --title scratch-terminal"++n) (title =? ("scratch-terminal"++n))  manageTerm | n <- map show [1..3]]
             ++ [ NS "clementine" "clementine" (className =? "Clementine") (proportionalCenteredSomething 0.9)
                , NS "telegram" "telegram" (className =? "TelegramDesktop") (proportionalCenteredSomething 0.9)
                , NS "thunderbird" "thunderbird" (className =? "Thunderbird") (proportionalCenteredSomething 0.9)
                ]

manageSomething h w t l = customFloating $ W.RationalRect l t w h
centeredSomething h w = manageSomething h w ((1-h)/2) ((1-h)/2)
-- Same proportion as my monitor, only one variable to make shit easier
-- ratio of the screen occupied
proportionalCenteredSomething s = manageSomething s s ((1-s)/2) ((1-s)/2)

manageTerm = manageSomething h w t l
    where
        h = 0.5
        w = 0.5
        t = (1-h)/2 * 1.6
        l = (1-h)/2 * 1.9

-- I wanted mpv to open the youtube link on my clipboard and share that
-- as a small window on all workspaces. Pretty neat!
spawnSharedMpv = do
    possibleLink <- getClipboardString
    when (possibleLink /= Nothing) $ do
        spawn $ "mpv --title=sharedMpv " ++ fromJust possibleLink

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_t), spawn $ XMonad.terminal conf)
    , ((modm, xK_p), spawn $ (XMonad.terminal conf) ++ " -e ranger")
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_q), kill)
    , ((modm, xK_r), spawn "rofi -show run")
    , ((modm, xK_c), spawn "firefox")
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm, xK_s), withFocused $ windows . W.sink)
    , ((modm,xK_m), windows W.swapMaster)
    , ((modm, xK_comma ), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_e), io (exitWith ExitSuccess))
    , ((shiftMask, xK_F1), spawn lowerVolume)
    , ((shiftMask, xK_F2), spawn raiseVolume)
    , ((mod4Mask, xK_m), liftIO spawnSharedMpv)
    , ((mod4Mask, xK_c), windows copyToAll)
    , ((mod4Mask, xK_t), namedScratchpadAction myScratchPads "telegram")
    , ((mod4Mask, xK_e), namedScratchpadAction myScratchPads "thunderbird")
    , ((shiftMask, xK_F3), namedScratchpadAction myScratchPads "clementine")
    , ((shiftMask, xK_F9), namedScratchpadAction myScratchPads "terminal1")
    , ((shiftMask, xK_F10), namedScratchpadAction myScratchPads "terminal2")
    , ((shiftMask, xK_F11), namedScratchpadAction myScratchPads "terminal3")
    , ((0, xK_Print), spawn "scrot")
    , ((modm,               xK_space ), sendMessage NextLayout)
    ]

    ++

    [ ((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myGaps = [(U,18), (D,18), (L, 18), (R,18)]

goldenRatio :: Rational
goldenRatio = realToFrac $ 2/(1 + sqrt 5)

myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = goldenRatio

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myStartupHook :: X ()
myStartupHook = do
    spawn "compton -c --backend glx --vsync opengl-swc"
    spawn "xset m 0 0"
    spawn "feh --bg-scale \"$(cat .config/i3/wallpaperlocation)\""
    spawn "xsetroot -cursor_name left_ptr"

myManageHooks = composeAll
                [ manageDocks
                --, fullscreenManageHook
                , title =? "sharedMpv" --> doRectFloat (W.RationalRect (1 % 6) (1 % 6) (1 % 4) (1 % 4))
                , namedScratchpadManageHook myScratchPads
                , fullscreenManageHook
                ]

-------------------------

main = xmonad =<< xmobar defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        manageHook = myManageHooks <+> manageHook defaultConfig,
        --layoutHook = avoidStruts  $  layoutHook defaultConfig,

        layoutHook         = myLayout,
        startupHook = myStartupHook

    }

-------------------------
