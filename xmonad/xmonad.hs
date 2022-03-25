{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}


import Control.Monad
import Data.Function.Flippers
import Data.List   ( zip4, mapAccumL )
import Data.Maybe  ( fromJust )
import System.Environment ( setEnv )
import System.Exit ( exitSuccess )
import Text.Format
import XMonad

import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Search
import XMonad.Actions.ShowText
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode       ( orgPrompt )
import XMonad.Prompt.Shell
import XMonad.Prompt.Unicode

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified Data.Map          as M
import qualified XMonad.StackSet   as W

myTerminal = "kitty"
myModMask  = mod4Mask
altMask    = mod1Mask

myWorkspaces =
    [ "term"
    , "www"
    , "dir"
    , "mus"
    , "docs"
    , "free"
    , "call"
    , "vid"
    , "chat"
    , "dev"
    , "hid" -- always hidden, this is for dropdown terminal
    ]
myWorkspaceIDs = M.fromList $ zip myWorkspaces $ [1..9] ++ [0]

getWorkspacesApps term =
    [ term
    , "qutebrowser --qt-flag ignore-gpu-blacklist"
    ++ " --qt-flag enable-gpu-rasterization"
    ++ " --qt-flag enable-native-gpu-memory-buffers"
    ++ " --qt-flag num-raster-threads=4"
#ifdef PC
    , "thunar"
#else
    , term ++ " -e ranger"
#endif
    , "spotify"
    , "zathura"
    , term
    , "skypeforlinux"
    , "vlc"
    , "discord"
    , term ++ " -e nvim"
    ]

mySHC = def
    { st_font = "xft:FiraCode:size=30:weight=semibold"
    , st_bg   = "#24283B"
    , st_fg   = "#C9C1D6"
    }

mySHCIcons = mySHC
    { st_font = "xft:Font Awesome 5 Pro-Solid:size=60:weight=bold"
    }

mySearchEngines = M.fromList
    [ ("a", "https://wiki.archlinux.org/index.php?search={0}")
    , ("y", "https://www.youtube.com/results?search_query={0}")
    , ("r", "https://www.reddit.com/r/{0}/")
    , ("d", "https://duckduckgo.com/?q={0}")
    , ("w", "https://en.wikipedia.org/wiki/{0}")
    , ("g", "https://www.google.com/search?q={0}")
    , ("DEFAULT", "https://www.google.com/search?q={0}")
    ]

searchFunc :: String -> String
searchFunc s
    | "http://"  `isPrefixOf` s = s
    | "https://" `isPrefixOf` s = s
    | "www."     `isPrefixOf` s = s
    | otherwise = format (mySearchEngines M.! prefix) [escape rest]
  where
    (prefix, rest) = case take 2 s of
        [x, ' '] -> if [x] `M.member` mySearchEngines
                    then ([x], tail $ tail s)
                    else ("DEFAULT", s)
        _ -> ("DEFAULT", s)

mySearchEngine = searchEngineF "" searchFunc

currScreenID :: X ScreenId
currScreenID = gets $ W.screen . W.current . windowset

----------------------------------------------------------------------
-- BEGIN BINDINGS
myKeys conf@XConfig { XMonad.modMask    = modm
                    , XMonad.terminal   = term
                    , XMonad.workspaces = workspaces''
                    } = M.fromList $
    let flashText_ c = flashText c (3/4)
        runProcessAndTrim p f i = trim <$> runProcessWithInput p f i
        workspacesApps = getWorkspacesApps term
        workspaces = workspaces' conf
    in
    -- function keys
    [ ((0, xF86XK_MonBrightnessUp)
       , runProcessWithInput "lux" ["-a", "5%"] ""
       >> runProcessAndTrim "lux" ["-G"] ""
       >>= flashText_ mySHC . ("Brightness: "++))

    , ((0, xF86XK_MonBrightnessDown)
       , runProcessWithInput "lux" ["-s", "5%"] ""
       >> runProcessAndTrim "lux" ["-G"] ""
       >>= flashText_ mySHC . ("Brightness: "++))

    , ((0, xF86XK_AudioRaiseVolume)
       , runProcessWithInput "amixer"
       ["-qD", "pulse", "sset", "Master", "2%+"] ""
       >> runProcessAndTrim "pamixer" ["--get-volume"] ""
       >>= flashText_ mySHC . ("Volume: "++) . (++"%"))

    , ((0, xF86XK_AudioLowerVolume)
       , runProcessWithInput "amixer"
       ["-qD", "pulse", "sset", "Master", "2%-"] ""
       >> runProcessAndTrim "pamixer" ["--get-volume"] ""
       >>= flashText_ mySHC . ("Volume: "++) . (++"%"))

    , ((0, xF86XK_AudioMute)
       , runProcessAndTrim "pamixer" ["--get-mute"] ""
       >>=  uncurry flashText_ . (\case
           "false" -> (mySHCIcons { st_fg = "#FF4C6B" }, "\xf6a9")
           _       -> (mySHCIcons { st_fg = "#90A050" }, "\xf6a8"))
       >> spawn "amixer -qD pulse sset Master toggle")

    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPause), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_AudioStop), spawn "playerctl stop")
    , ((modm, xK_F1), spawn "playerctl previous")
    , ((modm, xK_F2), spawn "playerctl play-pause")
    , ((modm, xK_F3), spawn "playerctl next")
    ]
    ++
    -- Spawn certain apps on certain workspaces
    let runAndShift w c = sequence_ [spawnOn w c, windows $ onCurrentScreen W.view w]
    in
    [ ( (modm .|. m, k)
      , case q of
          Nothing -> runAndShift w c
          Just x -> raiseMaybe (runAndShift w c) (className =? x)
      )
    | ((m, k, q), w, c) <- zip3
        [ (0,       xK_Return, Nothing           ) -- term
        , (0,       xK_w,      Just "qutebrowser") -- web
        , (0,       xK_r,      Just "Thunar"     ) -- files
        , (0,       xK_s,      Just "Spotify"    ) -- music
        , (altMask, xK_o,      Nothing           ) -- documents
        , (altMask, xK_Return, Nothing           ) -- free
        , (altMask, xK_s,      Just "Skype"      ) -- calls
        , (0,       xK_v,      Just "vlc"        ) -- video
        , (0,       xK_d,      Just "discord"    ) -- chat
        , (0,       xK_b,      Nothing           ) -- development
        ]
        workspaces
        workspacesApps
    ]
    ++
    -- With ctrl, spawn the app on current workspace
    [ ((modm .|. controlMask, k), spawn c)
    | (k, c) <- zip
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
    ]
    ++
    -- Quick web search
    [ ((modm .|. shiftMask, xK_o), promptSearchBrowser
        myPromptConfig { showCompletionOnTab = True
                       , defaultPrompter = const "Quick search: "
                       }
        "vimb" mySearchEngine
      )
    , ((modm, xK_o), inputPrompt myPromptConfig "Web search"
        ?+ \s -> windows
                 (W.view (workspaces !! 1))
                 >> liftIO getBrowser
                 >>= flip3 search s searchFunc
      )
    , ((modm .|. shiftMask, xK_g), windows
        (W.view (workspaces !! 1))
        >> selectSearch mySearchEngine)
    -- Open shell prompt
    , ((modm, xK_p), shellPrompt myPromptConfig)
    -- Increase opacity
    , ((modm .|. controlMask, xK_Up), spawn "picom-trans -c -o -5")
    -- Screenshot
    , ((0, xK_Print), spawn "flameshot gui")
    -- Decrease opacity
    , ((modm .|. controlMask, xK_Down), spawn "picom-trans -c -o +5")
    -- mod-F12 toggles 'free' workspace
    , ((modm, xK_F12), toggleOrView "free")
    -- Launch rofi
    , ((modm .|. controlMask, xK_p), spawn "rofi -show run")
    -- Launch rofi window
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show window")
    -- Close focused window
    , ((modm, xK_c), kill)
    -- Copy window to all workspaces (for floating skype window)
    -- , ((modm .|. altMask, xK_c), do
        -- let compM f mg = mg >>= \g -> pure $ f . g
        -- let marshall_ = marshall <$> currScreenID
        -- copy_ <- copy `compM` marshall_
        -- windows \s -> mapM (flashText_ mySHC . show . W.tag) (W.workspaces s)
        -- -- windows (\s -> foldr (copy_ . W.tag) s (W.workspaces s))
        -- -- windows copyToAll
      -- )
    -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)
    -- Toggle fullscreen
    , ((0, xK_F11), sendMessage ToggleLayout)
    -- Shrinkg window
    , ((modm, xK_a), sendMessage MirrorShrink)
    -- Grow window
    , ((modm, xK_z), sendMessage MirrorExpand)
    -- Shrink the master area
    , ((modm, xK_h), sendMessage Expand)
    -- Expand the master area
    , ((modm, xK_l), sendMessage Shrink)
    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)
    -- Spawn floating terminal
    , ((modm .|. shiftMask, xK_Return), spawn "kitty --class=kitty-float")
    -- Spawn floating facebook messenger
    , ((modm .|. shiftMask, xK_m), spawn "caprine")
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Toggle bluettoth
    , ((modm .|. altMask, xK_b), spawn "bluetooth toggle")
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q)
       , confirmPrompt myPromptConfig "exit?" $ io exitSuccess)
    -- Shutdown
    , ((modm .|. shiftMask .|. controlMask, xK_q)
       , confirmPrompt myPromptConfig "shutdown?"
       $ spawn "shutdown +0")
    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile && xmonad --restart")
    -- Simulates drop-down terminal
    , ((modm, xK_backslash), ifWindow
        (className =? "kitty-dropdown")
        (ask >>= doF
        . (\a s -> if a `elem` W.index s
                   then W.shiftWin (marshall 0 "hid") a s
                   else W.shiftWin (W.currentTag s) a s
          )
        )
        (spawnHere "kitty --class=kitty-dropdown"))
    ]
    ++
    -- Rotate through workspaces using j and k
    -- Shift window on the way using h and l
    [ ((modm .|. controlMask, k), sequence_ (f1 : f2))
        | (k, f1, f2) <- zip3
            [xK_k, xK_j, xK_h, xK_l]
            [nextWS, prevWS, shiftToPrev, shiftToNext]
            [[], [], [prevWS], [nextWS]]
    ]
    ++
    -- mod-[1..0], toggles workspace N
    -- mod-shift-[1..0], move client to workspace N
    let
    shiftAndFocus i = W.view i . W.shift i
#ifdef PC
    toggle i = do
        curr <- gets $ W.currentTag . windowset
        last <- head <$> gets (map W.tag . W.hidden . windowset)
        wanted <- marshall <$> currScreenID <*> pure i
        let next = if curr == wanted then last else wanted
        windows $ W.view next
#else
    toggle = toggleOrDoSkip ["hid"] W.greedyView
#endif
    in
    [ ((m .|. modm, k), f i)
        | (i, k) <- zip workspaces $ [xK_1..xK_9] ++ [xK_0]
        , (f, m) <- [ (toggle, 0)
                    , (windows . onCurrentScreen shiftAndFocus, shiftMask)
                    , (windows . onCurrentScreen W.shift, controlMask)
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

-- END BINDINGS
----------------------------------------------------------------------
myMouseBindings XConfig { XMonad.modMask = modm } =
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
myPromptConfig = def
    { promptBorderWidth = 0
#ifdef PC
    , height            = 46
    , font              = "xft:FiraCode:size=14:weight=semibold"
#else
    , height            = 36
    , font              = "xft:FiraCode:size=12:weight=semibold"
#endif
    , position          = Top
    , bgColor           = "#24283B"
    , fgColor           = "#C9C1D6"
    , searchPredicate   = fuzzyMatch
    , sorter            = fuzzySort
    }

----------------------------------------------------------------------
myLayout = avoidStruts
    $ smartBorders
    $ windowNavigation
#ifdef PC
    $ mySpacing 16
#else
    $ mySpacing 8
#endif
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
    mySpacing x = spacingRaw
        False (Border x x x x) True (Border x x x x) True

----------------------------------------------------------------------
myManageHook = composeAll
    $ let
        doCenter = doRectFloat
            $ W.RationalRect x x (1 - 2 * x) (1 - 2 * x)
#ifdef PC
        x = (1/4)
#else
        x = (1/8)
#endif
        centered = [ "Vimb"
                   , "Skype"
                   , "Caprine"
                   , "kitty-float"
                   , "qBittorrent"
                   , "Gpick"
                   , "gpick"
                   ]
#ifdef PC
            ++ ["Spotify", "netflix", "Steam", "spotify", "discord", "Thunar"]
#else
            ++ ["kitty-dropdown"]
#endif
        floating = ["ij-ImageJ", "ImageJ"]
        ignored  = ["desktop_window"]

    in [className =? cls --> doFloat  | cls <- floating]
    ++ [className =? cls --> doCenter | cls <- centered]
    ++ [className =? cls --> doIgnore | cls <- ignored ]
    ++ [stringProperty "WM_NAME" =? "Zoom Meeting" --> doCenter]
    ++ [stringProperty "WM_NAME" =? "Zoom Cloud Meetings" --> doCenter]
#ifdef PC
    ++ [className =? "kitty-dropdown" --> doRectFloat (W.RationalRect (1/14) (1/3) (3/8) (4/9))]
#endif

----------------------------------------------------------------------
-- The rest is managed in ~/.xsession
myStartupHook = spawnOnce "picom --experimental-backends &"
#ifndef PC
    >> spawnOnce "xautolock -time 10 -locker 'screensaver' &"
#endif
    >> spawnNOnOnce 2 "term" myTerminal

----------------------------------------------------------------------
myLogHook proc = dynamicLogWithPP
#ifdef PC
    $ marshallPP 0
#endif
    $ xmobarPP
        { ppOutput          = hPutStrLn proc
        , ppCurrent         = xmobarColor "#FFCC10" "" . prepareWS
        , ppHidden          = xmobarColor "#CA65F9" "" . prepareWS
        , ppVisible         = xmobarColor "#FF6C6B" "" . prepareWS
        , ppHiddenNoWindows = xmobarColor "#4594BF" "" . prepareWS
        , ppTitle           = mempty
        , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
        , ppOrder           = \(ws:l:t:ex) -> (ws:l:ex) ++ [t]
        , ppSep             = wrap space doubleSpace
            $ xmobarColor "#999999" "" "|"
        , ppWsSep           = wrap space space
            $ xmobarColor "#555555" "" "|"
        , ppLayout          = xmobarColor "#FF4854" ""
            . wrap "<action=xdotool key super+space>" "</action>"
            . last
            . words
        }
  where
    space = "<fn=4> </fn>"
    doubleSpace = space ++ space
    icons = M.fromList
        [ ("term", "<fn=1>\xf120</fn>")
        , ("www" , "<fn=1>\xf719</fn>")
        , ("dir" , "<fn=1>\xf660</fn>")
        , ("mus" , "<fn=1>\xf001</fn>")
        , ("docs", "<fn=1>\xf15b</fn>")
        , ("free", "<fn=1>\xf78a</fn>")
        , ("call", "<fn=1>\xf095</fn>")
        , ("vid" , "<fn=1>\xf03d</fn>")
        , ("chat", "<fn=1>\xf086</fn>")
        , ("dev" , "<fn=1>\xf126</fn>")
        ]
    prepareWS name
        | name `M.notMember` icons = mempty
        | otherwise = "<action=xdotool key super+"
            ++ show (myWorkspaceIDs M.! name)
            ++ ">"
            ++ icons M.! name
            ++ "</action>"

----------------------------------------------------------------------
myHandleEventHook = handleTimerEvent <+> swallowEventHook
    (    className =? "kitty"
    <||> className =? "Alacritty"
    <||> className =? "st-256color")
    (pure True)

----------------------------------------------------------------------
myConfig logHandle = def
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , borderWidth        = 0
    , modMask            = myModMask
#ifdef PC
    , workspaces         = withScreens 2 myWorkspaces
#else
    , workspaces = myWorkspaces
#endif
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = manageSpawn
        <+> myManageHook
        <+> manageDocks
    , logHook            = myLogHook logHandle
    , startupHook        = myStartupHook
    , handleEventHook    = myHandleEventHook
    }

----------------------------------------------------------------------
main = setEnv "BROWSER" "qutebrowser"
    >> setEnv "EDITOR" "nvim"
    >> spawnPipe "/home/martin/.local/bin/xmobar"
    >>= xmonad . docks . ewmh . ewmhFullscreen . myConfig

