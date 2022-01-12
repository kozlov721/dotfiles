{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

import Data.Function.Flippers
import Data.List   ( zip4 )
import Data.Maybe  ( fromJust )
import System.Exit ( exitSuccess )
import System.Environment ( setEnv )
import Text.Format
import XMonad

import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Search
import XMonad.Actions.ShowText
import XMonad.Actions.SpawnOn

import XMonad.Config.Dmwit ( altMask )

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
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
myModMask = mod4Mask

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
    ]

myWorkspaceIDs = M.fromList $ zip myWorkspaces $ [1..9] ++ [0]

workspacesApps =
    [ myTerminal
    , "qutebrowser"
    , myTerminal ++ " -e ranger"
    , "spotify"
    , "zathura"
    , myTerminal
    , "skypeforlinux"
    , "vlc"
    , "discord"
    , myTerminal ++ " -e nvim"
    ]

mySHC = def
    { st_font = "xft:FiraCode:size=30:weight=semibold"
    , st_bg = "#24283B"
    , st_fg = "#C9C1D6"
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

----------------------------------------------------------------------
-- BEGIN BINDINGS
myKeys conf@XConfig { XMonad.modMask = modm } =
    M.fromList $
    let flashText_ c str = flashText c (3/4) str
        runProcessAndTrim p f i = trim <$> runProcessWithInput p f i
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
    , ((modm, xK_F1), spawn "playerctl previous")
    , ((modm, xK_F2), spawn "playerctl play-pause")
    , ((modm, xK_F3), spawn "playerctl next")
    ]
    ++
    -- Spawn certain apps on certain workspaces
    [ ( (modm .|. m, k)
      , sequence_ [spawnOn w c, windows (W.greedyView w)]
      )
    | ((m, k), w, c) <- zip3
        [ (0, xK_Return)       -- term
        , (0, xK_w)            -- web
        , (0, xK_r)            -- files
        , (0, xK_s)            -- music
        , (altMask, xK_o)      -- documents
        , (altMask, xK_Return) -- free
        , (altMask, xK_s)      -- calls
        , (0, xK_v)            -- video
        , (0, xK_d)            -- chat
        , (0, xK_b)            -- development
        ]
        (XMonad.workspaces conf)
        workspacesApps
    ]
    ++
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
    ]
    ++
    -- Quick web search
    [ ((modm .|. shiftMask, xK_o), promptSearchBrowser
        myPromptConfig { showCompletionOnTab = True
                       , defaultPrompter = const "Quick search: "}
        "vimb" mySearchEngine
      )
    , ((modm, xK_o), inputPrompt myPromptConfig "Web search"
        ?+ \s -> windows
                 (W.greedyView (XMonad.workspaces conf !! 1))
                 >> liftIO getBrowser
                 >>= flip3 search s searchFunc
      )
    , ((modm .|. shiftMask, xK_g), windows
        (W.greedyView (XMonad.workspaces conf !! 1))
        >> selectSearch mySearchEngine)
    -- Open man prompt
    , ((modm, xK_F1), manPrompt myPromptConfig)
    -- Open shell prompt
    , ((modm, xK_p), shellPrompt myPromptConfig)
    -- Open org prompt
    , ((modm .|. controlMask, xK_o), orgPrompt
        myPromptConfig "TODO" "/home/martin/org/todo")
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
    let shiftAndFocus i = W.greedyView i . W.shift i in
    [ ((m .|. modm, k), f i)
        | (i, k) <- zip
            (XMonad.workspaces conf)
            $ [xK_1..xK_9] ++ [xK_0]
        , (f, m) <- [ (toggleOrView, 0)
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
    { font              = "xft:FiraCode:size=12:weight=semibold"
    , promptBorderWidth = 0
    , height            = 36
    , position          = Top
    , bgColor           = "#24283B"
    , fgColor           = "#C9C1D6"
    }


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
    mySpacing x = spacingRaw
        False (Border x x x x) True (Border x x x x) True

----------------------------------------------------------------------
myManageHook = composeAll
    $ let
        doCenter = doRectFloat
            $ W.RationalRect p p (1 - 2 * p) (1 - 2 * p)
#ifdef PC
        p = (1/5)
#else
        p = (1/8)
#endif
        centered = ["Vimb", "Skype", "Caprine", "kitty-float"]
        floating = ["ij-ImageJ", "ImageJ"]
        ignored  = ["desktop_window"]

    in [className =? cls --> doFloat  | cls <- floating]
    ++ [className =? cls --> doCenter | cls <- centered]
    ++ [className =? cls --> doIgnore | cls <- ignored ]

----------------------------------------------------------------------
-- The rest is managed in ~/.xsession
myStartupHook = spawnOnce "picom --experimental-backends &"
    >> spawnOnce "xautolock -time 10 -locker 'screensaver' &"
    >> spawnNOnOnce 2 "term" myTerminal

----------------------------------------------------------------------
myLogHook proc = dynamicLogWithPP
    $ xmobarPP
        { ppOutput          = hPutStrLn proc
        , ppCurrent         = xmobarColor "#FFCC10" "" . prepareWS
        , ppHidden          = xmobarColor "#CA65F9" "" . prepareWS
        , ppHiddenNoWindows = xmobarColor "#4594BF" "" . prepareWS
        , ppTitle           = const ""
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
    space = "<fn=5> </fn>"
    doubleSpace = space ++ space
    icons = M.fromList
        [ ("term", "<fn=2>\xf120</fn>")
        , ("www" , "<fn=2>\xf719</fn>")
        , ("dir" , "<fn=2>\xf660</fn>")
        , ("mus" , "<fn=2>\xf001</fn>")
        , ("docs", "<fn=2>\xf15b</fn>")
        , ("free", "<fn=2>\xf78a</fn>")
        , ("call", "<fn=2>\xf095</fn>")
        , ("vid" , "<fn=2>\xf03d</fn>")
        , ("chat", "<fn=2>\xf086</fn>")
        , ("dev" , "<fn=2>\xf126</fn>")
        ]
    prepareWS name = "<action=xdotool key super+"
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
    , workspaces         = myWorkspaces
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
    >> spawnPipe "/home/martin/.config/xmobar/xmobar"
    >>= xmonad . docks . ewmh . ewmhFullscreen . myConfig

