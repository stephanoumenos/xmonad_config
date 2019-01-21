import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "xfce4-terminal --hide-menubar"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth = 2

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
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
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

myStartupHook :: X ()
myStartupHook = do
    spawn "compton -c --backend glx --vsync opengl-swc"
    spawn "xset m 0 0"
    spawn "feh --bg-scale \"$(cat .config/i3/wallpaperlocation)\""

myManageHooks = composeAll
    [ manageDocks
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
        layoutHook = avoidStruts  $  layoutHook defaultConfig,

        startupHook = myStartupHook

    }

-------------------------


help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
