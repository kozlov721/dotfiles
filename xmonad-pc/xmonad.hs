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
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle as MToggle
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Default terminal
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask = mod4Mask

myWorkspaces = ["term", "www", "dir", "mus", "docs",
                "free", "call", "vid", "chat", "dev"]

workspacesApps = [myTerminal, "qutebrowser", "alacritty -e ranger",
                  "spotify", "onlyoffice-desktopeditors", myTerminal,
                  "skypeforlinux", "vlc", "discord", "code"]

-- For 'clickable' function
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces $ [1..9] ++ [0]


myBorderWidth = 4

myNormalBorderColor  = "#ffffff"

myFocusedBorderColor = "#ce2d52"
-- myFocusedBorderColor = "#1939b9"


--------------------- Key Bindings ----------------------
---------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Special function keys
    [ ((0, xF86XK_AudioRaiseVolume), spawn "amixer -qD pulse sset Master 2%+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -qD pulse sset Master 2%-")
    , ((0, xF86XK_AudioMute), spawn "amixer -qD pulse sset Master toggle")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((modm, xK_F1), spawn "playerctl previous")
    , ((modm, xK_F3), spawn "playerctl next")
    ]
    ++
    -- Spawn certain apps on certain workspaces
    [((modm .|. m, k), sequence_ [spawnOn w c, windows (W.greedyView (w))])
      | (k, m, w, c) <- zip4
        [xK_Return, xK_w, xK_r, xK_s, xK_o, xK_Return, xK_s, xK_v, xK_d, xK_b]
        [0, 0, 0, 0, 0, altMask, altMask, 0, 0, 0]
        (XMonad.workspaces conf)
        workspacesApps
    ]
    ++
    -- With ctrl, spawn the app on current workspace
    [((modm .|. controlMask, k), spawn c)
      | (k, c) <- zip
        [xK_Return, xK_w, xK_r, xK_s, xK_o,
         xK_Return, xK_m, xK_v, xK_d, xK_b]
        workspacesApps
    ]
    ++
    -- Somehow simulates drop down terminal on F12 key
    -- (runs floating st, kills it if it runs already)
    [  ((0, xK_F12), spawn "~/.local/bin/dropdown-term")

    -- Toggles light theme
    ,  ((modm .|. controlMask .|. shiftMask, xK_l)
        , spawn "python3 ~/.local/bin/theme-changer")

    -- retoggle skype floating window
    ,  ((modm .|. altMask, xK_l), sequence_ [windows (W.greedyView (XMonad.workspaces conf !! 6)), toggleWS])

    -- Increase opacity
    ,  ((modm .|. controlMask, xK_Up), spawn "picom-trans -c -o -5")

    -- Decrease opacity
    ,  ((modm .|. controlMask, xK_Down), spawn "picom-trans -c -o +5")

    -- mod-F12 toggles 'free' workspace
    ,  ((modm, xK_F12), toggleOrView "free")

    -- Launch dmenu
    ,  ((modm, xK_p), spawn "dmenu_run")

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
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

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

    -- Spawn floating facebook messenger
    , ((modm .|. shiftMask, xK_m), spawn "caprine")
    , ((altMask, xK_F12), spawn "caprine")

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- Reflect the layout vertically
    , ((modm .|. controlMask, xK_x), sendMessage $ MToggle.Toggle REFLECTX)

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
    ]
    ++
    -- Rotate through workspaces using j and k
    -- Shift window on the way using h and l
    [ ((modm .|. controlMask, k), sequence_ (f1:f2))
      |  (k, f1, f2) <- zip3 [xK_k, xK_j, xK_h, xK_l]
                             [nextWS, prevWS, shiftToPrev, shiftToNext]
                             [[], [], [prevWS], [nextWS]]
    ]
    ++
    -- mod-[1..0], toggles workspace N
    -- mod-shift-[1..0], move client to workspace N, keep focus on the moved window
    let shiftAndFocus i = W.greedyView i . W.shift i in
    [ ((m .|. modm, k), f i)
      | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [(toggleOrView, 0), (windows . shiftAndFocus, shiftMask), (windows . W.shift, controlMask)]
    ]
    ++
    -- Special bindings to run cbonsai as screensaver
    -- Not to be used by the user
    [ ((modm .|. controlMask .|. shiftMask, xK_s), sequence_ [appendWorkspace "saver"])
    , ((modm .|. controlMask .|. shiftMask, xK_w), sequence_ [kill, toggleWS, removeWorkspaceByTag "saver"])
    ]


----------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((0, 9), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> withFocused $ windows . W.sink)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    , ((0, 8), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

----------------------------------------------------------------------
-- Layouts:

mySpacing x = spacingRaw False (Border x x x x) True (Border x x x x) True

myLayout = mkToggle (single REFLECTX)
            $ avoidStruts
            $ smartBorders
            $ windowNavigation
            $ mySpacing 16
            $ toggleLayouts Full myTiled    |||
              toggleLayouts Full myMirrored |||
              toggleLayouts Full Grid
    where
        myTiled = renamed [Replace "Tall"]
            $ reflectHoriz
            $ ResizableTall 1 (3 / 100) (1 / 2) []
        myMirrored = renamed [Replace "Mirr"]
            $ Mirror
            $ ResizableTall 1 (3 / 100) (1 / 2) []


----------------------------------------------------------------------
-- Window rules:


myManageHook = composeAll
    [ className =? "ImageJ"           --> doFloat
    , className =? "Skype"            --> doFloat
    , className =? "st-256color"      -->
      let {dx = (2/7); dy = (1/5)}
      in doRectFloat (W.RationalRect dx dy (1 - 2 * dx) (2 * dy))
    , className =? "Surf"             --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , className =? "Gpick"            --> doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , className =? "Caprine"          --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , resource  =? "desktop_window"   --> doIgnore ]

----------------------------------------------------------------------
--
-- Event handling
myEventHook = docksEventHook <+> fullscreenEventHook


clickable :: String -> String
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices


filterSaver :: String -> String
filterSaver str = if isInfixOf saverText strText then "" else str
            where
                saverText = pack "saver"
                strText = pack str

----------------------------------------------------------------------
--
-- Startup hook
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "numlockx on"
    spawnOnce "dropbox &"
    spawnOnce "setxkbmap -layout cz coder"
    spawnOnce "picom --experimental-backends &"
    spawnOnce "xset r rate 330 30"
    spawnOnce "imwheel -kill"
    -- spawnOnce "xautolock -time 10 -locker 'screensaver' &"
    -- spawnOnce "xautolock -time 20 -locker 'slock' &"
    -- spawnOnce "xinput set-prop 'ELAN2602:00 04F3:3109 Touchpad' 'libinput Natural Scrolling Enabled' 1"
    spawnNOnOnce 2 "term" myTerminal

----------------------------------------------------------------------

main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
	xmonad $ ewmh def {
          terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , clickJustFocuses   = myClickJustFocuses
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        -- key bindings
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        -- hooks, layouts
        , layoutHook         = myLayout
        , manageHook         = manageSpawn <+> myManageHook <+> manageDocks
        , handleEventHook    = myEventHook
        , logHook            = let separator = "<fc=#555555><fn=1>|</fn></fc> " in
            dynamicLogWithPP $ xmobarPP {
              ppOutput = \x -> hPutStrLn xmproc x
            -- Current workspace
            , ppCurrent = xmobarColor "#CCE01B" "" . wrap (separator ++ "[ ") " ]" . filterSaver

            -- Hidden workspaces
            , ppHidden = xmobarColor "#C792EA" "" . wrap separator "" . clickable . filterSaver

            -- Hidden workspaces (no windows)
            , ppHiddenNoWindows = xmobarColor "#82AAFF" "" . wrap separator ""  . clickable . filterSaver

            -- Title of active window
            , ppTitle = \x -> ""

            -- Separator character
            , ppSep =  " <fc=#999999><fn=1>| </fn></fc> "

            -- Type of layout
            , ppLayout = wrap "<action=xdotool key super+space>" "</action>" . last . words

            -- Urgent workspace
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"

            , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }

        , startupHook        = myStartupHook
    }

