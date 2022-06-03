{-# LANGUAGE CPP #-}

import MyPlugins
import Xmobar
import Text.Format

#ifdef PC
mainFontSize       = 15 :: Int
additionalFontSize = 14 :: Int
#else
mainFontSize       = 13 :: Int
additionalFontSize = 12 :: Int
#endif

myCommands :: [Runnable]
myCommands =
    [ Run $ MyBattery "battery" 50
    , Run $ Cpu [ "-t"
                , "<fn=1>\xf108</fn>" <--> "<total>%"
                , "-H"
                , "50"
                , "--high"
                , "red"
                ] 20
    , Run $ Memory ["-t", "<fn=2>\xf538</fn>"
        ++ doubleSpace
        ++ "<usedratio>%"] 20
    , Run $ MyVolume "volume" 1
    , Run $ Bluetooth "bluetooth"
    , Run $ Pacman "pacupdate" 1800
    , Run $ Date
        (    "%A,"
        <--> "%B"
        <--> "%d,"
#ifdef PC
        <--> "%Y,"
#endif
        <--> "%T"
        ) "date" 10
    , Run $ Wireless "" ["-t", "<ssid>" <-> "<quality>"] 50
    , Run UnsafeStdinReader
    ]

myTemplate :: String
myTemplate = doubleSpace
#ifdef PC
    ++ space
#endif
    ++  lambda
    ++  space
    <|> "%UnsafeStdinReader%"
    <-> "}"
    ++  time
    ++  "{"
    ++  wifi
    <|> cpu
    <|> mem
    <|> volume
    <|> upd
#ifndef PC
    <|> "%bluetooth%"
    <|> "%battery%"
    ++  space
#else
    ++ doubleSpace
#endif
  where
    lambda  = fullWrap "#35749F" "rofi -show run"
               "<fn=1>\xf66e</fn>"
    time    = fullWrap "#EEAA10"
                (script "run-process calcurse")
                "%date%"
    wifi    = fullWrap "#EEAA00"
                (script "run-process nmtui")
                $ "<fn=1>\xf1eb</fn>" <-> "%wi%"
    cpu     = fullWrap "#E58030" htop $ space ++ "%cpu%"
    mem     = fullWrap "#FF6050" htop $ space ++ "%memory%"
    volume  = actionWrap mute "%volume%"
    upd     = actionWrap "kitty --class=kitty-float -e yay -Syu"
                "%pacupdate%"
    htop    = script "run-process btop"
    mute    = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

config :: Config
config = defaultConfig {
     font = format
              ("xft:FiraCode:size={0}:weight=bold"
              ++ ":hinting=true:antialias=true")
              [show mainFontSize]
    , additionalFonts = map (`format` [show additionalFontSize])
        [ -- for icons
          "xft:Font Awesome 5 Pro-Solid:size={0}:weight=bold"
        , "xft:Font Awesome 5 Pro-Regular:size={0}"
        , "xft:Font Awesome 5 Brands:size={0}"
          -- for spaces (necessary with monospace main font)
        , "xft:Roboto:size={0}:weight=semibold"
          ++ ":hinting=true:antialias=true"
        ]
    , bgColor      = "#473042"
    , fgColor      = "#FF6C6B"
    , alpha        = 240
    , position     = Top
    , lowerOnStart = True
    , hideOnStart  = False
    , persistent   = True
    , sepChar      = "%"
    , alignSep     = "}{"
    , commands     = myCommands
    , template     = myTemplate
}

main :: IO ()
main = xmobar config
