-- import Control.Monad (void)
import Data.List (zip4)
import Data.Maybe
import Data.Monoid
import Data.Text (isInfixOf, pack)
import Graphics.X11.ExtraTypes.XF86
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn

import XMonad.Config.Dmwit (altMask)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "kitty"

myFocusFollowsMouse = False

myClickJustFocuses = False

myModMask = mod4Mask

myWorkspaces =
    [ "term"
    , "www"
    , "dir"
    , "mus"
    , "docs"
    , "free"
    , "mail"
    , "vid"
    , "chat"
    , "dev"
    ]

workspacesApps =
    [ myTerminal
    , "qutebrowser"
    , myTerminal ++ " -e ranger"
    , "spotify"
    , "onlyoffice-desktopeditors"
    , myTerminal
    , myTerminal ++ " -e neomutt"
    , "vlc"
    , "discord"
    , "code"
    ]

myBorderWidth = 0

myNormalBorderColor = "#FFFFFF"

myFocusedBorderColor = "#CE2D52"

----------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList $
    -- function keys
    [ ((0, xF86XK_MonBrightnessUp), spawn "lux -a 2%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 2%")
    , ((0, xF86XK_AudioRaiseVolume)
       , spawn "amixer -qD pulse sset Master 2%+")
    , ((0, xF86XK_AudioLowerVolume)
       , spawn "amixer -qD pulse sset Master 2%-")
    , ((0, xF86XK_AudioMute)
       , spawn "amixer -qD pulse sset Master toggle")
    , ((modm, xK_F2), spawn "playerctl play-pause")
    , ((modm, xK_F1), spawn "playerctl previous")
    , ((modm, xK_F3), spawn "playerctl next")
    ]
    ++
    -- Spawn certain apps on certain workspaces
    [ ( (modm .|. m, k)
      , sequence_ [spawnOn w c, windows (W.greedyView w)])
    | (k, m, w, c) <-
          zip4
              [ xK_Return
              , xK_w
              , xK_r
              , xK_s
              , xK_o
              , xK_Return
              , xK_m
              , xK_v
              , xK_d
              , xK_b
              ]
              [0, 0, 0, 0, 0, altMask, altMask, 0, 0, 0]
              (XMonad.workspaces conf)
              workspacesApps
    ] ++
    -- With ctrl, spawn the app on current workspace
    [ ((modm .|. controlMask, k), spawn c)
    | (k, c) <-
          zip
              [ xK_Return
              , xK_w
              , xK_r
              , xK_s
              , xK_o
              , xK_Return
              , xK_m
              , xK_v
              , xK_d
              , xK_b
              ]
              workspacesApps
    ] ++
    -- Toggle 1st workspace
    [ ((0, xK_F12), toggleOrView "term")
    -- Toggle light theme
    , ( (modm .|. controlMask .|. shiftMask, xK_l)
      , spawn "fish -c 'change_theme'")
    -- Increase opacity
    , ((modm .|. controlMask, xK_Up), spawn "picom-trans -c -o -5")
    , ((0, xK_Print), spawn "flameshot gui")
    -- Decrease opacity
    , ((modm .|. controlMask, xK_Down), spawn "picom-trans -c -o +5")
    -- mod-F12 toggles 'free' workspace
    , ((modm, xK_F12), toggleOrView "free")
    -- Launch dmenu
    , ((modm, xK_p), spawn "rofi -show run")
    -- Launch rofi window
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show window")
    -- Close focused window
    , ((modm, xK_c), kill)
    -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)
    -- Toggle fullscreen
    , ((0, xK_F11), sendMessage ToggleLayout)
    -- Shrinkg window
    , ((modm, xK_a), sendMessage MirrorShrink)
    -- Grow window
    , ((modm, xK_z), sendMessage MirrorExpand)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space)
       , setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)
    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)
    -- Spawn floating terminal
    , ((modm .|. shiftMask, xK_Return), spawn "st")
    -- Spawn floating facebook messenger
    , ((modm .|. shiftMask, xK_m), spawn "caprine")
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
    , ((modm, xK_h), sendMessage Expand)
    -- Expand the master area
    , ((modm, xK_l), sendMessage Shrink)
    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io exitSuccess)
    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
    ] 
    ++
    -- Rotate through workspaces using j and k
    -- Shift window on the way using h and l
    [ ((modm .|. controlMask, k), sequence_ (f1 : f2))
        | (k, f1, f2) <-
            zip3
                [xK_k, xK_j, xK_h, xK_l]
                [nextWS, prevWS, shiftToPrev, shiftToNext]
                [[], [], [prevWS], [nextWS]]
    ]
    ++
    -- mod-[1..0], toggles workspace N
    -- mod-shift-[1..0], move client to workspace N
    let shiftAndFocus i = W.greedyView i . W.shift i in
    [ ((m .|. modm, k), f i)
        | (i, k) <- zip 
            (XMonad.workspaces conf)
            $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <-
            [ (toggleOrView, 0)
            , (windows . shiftAndFocus, shiftMask)
            , (windows . W.shift, controlMask)
            ]
    ] 
    ++
    -- Special bindings to run cbonsai as screensaver
    -- Not to be used by the user
    [ ((modm .|. controlMask .|. shiftMask, xK_s)
       , sequence_ [appendWorkspace "saver"])
    , ((modm .|. controlMask .|. shiftMask, xK_w)
       , sequence_ [toggleWS, removeWorkspaceByTag "saver"])
    ]

----------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) =
    M.fromList
        [ ((modm, button1)
          , \w -> focus w
                >> mouseMoveWindow w
                >> windows W.shiftMaster)
        , ((modm, button3)
          , \w -> focus w
                >> mouseResizeWindow w
                >> windows W.shiftMaster)
        ]

----------------------------------------------------------------------
myLayout = avoidStruts
    $ smartBorders 
    $ windowNavigation
    $ mySpacing 8 
    $ toggleLayouts Full myTiled
        ||| toggleLayouts Full myMirrored
        ||| toggleLayouts Full Grid
  where
    myTiled = renamed [Replace "Tall"]
        $ reflectHoriz
        $ ResizableTall 1 (3/100) (1/2) []
    myMirrored = renamed [Replace "Mirr"]
        $ Mirror
        $ ResizableTall 1 (3/100) (1/2) []
    mySpacing x =
        spacingRaw False (Border x x x x) True (Border x x x x) True

----------------------------------------------------------------------
myManageHook = composeAll
    $ let rect = W.RationalRect (1/6) (1/6) (2/3) (2/3) in
        [ className =? "ImageJ"         --> doFloat
        , className =? "st-256color"    --> doRectFloat rect
        , className =? "Surf"           --> doRectFloat rect
        , className =? "Gpick"          --> doRectFloat rect
        , className =? "Caprine"        --> doRectFloat rect
        , resource  =? "desktop_window" --> doIgnore
        ]

----------------------------------------------------------------------
-- The rest is managed in ~/.xsession
myStartupHook = do
    spawnOnce "picom --experimental-backends &"
    spawnOnce "xautolock -time 10 -locker 'screensaver' &"
    spawnNOnOnce 2 "term" myTerminal

----------------------------------------------------------------------
getIcon :: String -> String
getIcon str
    | str == "term" = "<fn=2>\xf120</fn>"
    | str == "www"  = "<fn=3>\xf719</fn>"
    | str == "dir"  = "<fn=3>\xf660</fn>"
    | str == "mus"  = "<fn=3>\xf001</fn>"
    | str == "docs" = "<fn=3>\xf15b</fn>"
    | str == "free" = "<fn=3>\xf78a</fn>"
    | str == "mail" = "<fn=3>\xf0e0</fn>"
    | str == "vid"  = "<fn=3>\xf03d</fn>"
    | str == "chat" = "<fn=3>\xf086</fn>"
    | str == "dev"  = "<fn=3>\xf126</fn>"
    | otherwise     = ""


clickable :: (String, String) -> String
clickable (name, icon) = "<action=xdotool key super+"
    ++ show index
    ++ ">"
    ++ icon
    ++ "</action>"
  where
    index   = fromJust $ M.lookup name indices
    indices = M.fromList $ zip myWorkspaces $ [1 .. 9] ++ [0]


myLogHook proc = dynamicLogWithPP
    $ xmobarPP
        { ppOutput          = hPutStrLn proc
        , ppCurrent         = xmobarColor "#FFCC10" "" . prepareWS
        , ppHidden          = xmobarColor "#C792EA" "" . prepareWS
        , ppHiddenNoWindows = xmobarColor "#82AAFF" "" . prepareWS
        , ppTitle           = const ""
        , ppSep             = " <fc=#999999><fn=1>|</fn></fc>   "
        , ppWsSep           = " <fc=#555555><fn=1>|</fn></fc> "
        , ppLayout          = wrap
            "<action=xdotool key super+space>" "</action>"
            . last . words
        , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
        , ppOrder           = \(ws:l:t:ex) -> [ws, l] ++ ex ++ [t]
        }
  where
    prepareWS str = clickable (str, getIcon str)

----------------------------------------------------------------------
myConfig logHandle = def
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = manageSpawn <+> myManageHook <+> manageDocks
    , logHook            = myLogHook logHandle
    , startupHook        = myStartupHook
    , handleEventHook    = fullscreenEventHook
    }

----------------------------------------------------------------------
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ ewmh $ myConfig xmproc

