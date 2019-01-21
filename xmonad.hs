import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
--import XMonad.Layout.Fullscreen

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "xfce4-terminal --hide-menubar"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth = 3

myModMask = mod1Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

myNormalBorderColor  = "dddddd"
myFocusedBorderColor = "#ff69b4"

lowerVolume =  "amixer -c 0 set Headphone 4- ; amixer -c 1 set Headphone 4- ; amixer -c 2 set Headphone 4-"
raiseVolume =  "amixer -c 0 set Headphone 4+ ; amixer -c 1 set Headphone 4+ ; amixer -c 2 set Headphone 4+"

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
    , ((modm,xK_m), windows W.swapMaster)
    , ((modm, xK_comma ), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_e), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_F1), spawn lowerVolume)
    , ((modm .|. shiftMask, xK_F2), spawn raiseVolume)
    , ((mod4Mask, xK_t), spawn "telegram-desktop")
    , ((modm, xK_F3), scratchpadSpawnActionCustom "chromium")
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

myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     -- Golden Ratio
     ratio   = realToFrac $ 2/(1 + sqrt 5)

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myStartupHook :: X ()
myStartupHook = do
    spawn "compton -c --backend glx --vsync opengl-swc"
    spawn "xset m 0 0"
    spawn "feh --bg-scale \"$(cat .config/i3/wallpaperlocation)\""

myManageHooks = composeAll
    [ manageDocks
    --, fullscreenManageHook
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
