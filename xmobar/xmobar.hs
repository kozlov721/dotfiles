import Xmobar
import MyPlugins


myCommands :: [Runnable]
myCommands = [
      Run $ MyBattery "battery" 50

    , Run $ Cpu [ "-t"
                , "<fn=2>\xf108</fn>  <total>%"
                , "-H"
                , "50"
                , "--high"
                , "red"
                ] 20

    , Run $ Memory ["-t", "<fn=3>\xf538</fn>  <usedratio>%"] 20

    , Run $ MyVolume "volume" 1

    , Run $ Bluetooth "bluetooth"

    , Run $ Pacman "pacupdate" 1800

    , Run $ Date "%A,  %B  %d,  %Y  %T" "date" 10

    , Run $ Wireless "" ["-t", "<ssid> <quality>"] 50

    , Run UnsafeStdinReader
    ]


-- Concatenates two strings with a bar separator in between
(+|+) :: String -> String -> String
str1 +|+ str2 = str1 ++ " " ++ sep ++ " " ++ str2
  where sep = colorWrap "#888888" "<fn=1>|</fn>"


myTemplate :: String
myTemplate = "  "
    ++  lambda
    +|+  "%UnsafeStdinReader% "
    ++  "}"
    ++  time
    ++  "{"
    ++  wifi
    +|+ cpu
    +|+ mem
    +|+ volume
    +|+ upd
    +|+ "%bluetooth%"
    +|+ "%battery%"
    ++  " "
  where
    lambda  = fullWrap "#35749F" "rofi -show run"
               "<fn=2>\xf66e</fn> "
    time    = fullWrap "#EEAA10"
                (script "run-process calcurse")
                "%date%"
    wifi    = fullWrap "#EEAA00"
                (script "run-process nmtui")
                "<fn=2>\xf1eb</fn> %wi%"
    cpu     = fullWrap "#E58030" htop " %cpu%"
    mem     = fullWrap "#FF6050" htop " %memory%"
    volume  = actionWrap mute "%volume%"
    upd     = actionWrap "st -e yay -Syu" "%pacupdate%"
    htop    = script "run-process btop"
    mute    = "pactl set-sink-mute @DEFAULT_SINK@ toggle"


config :: Config
config = defaultConfig {
      font="xft:Roboto:size=12:weight=semibold\
           \:hinting=true:antialias=true"
    , additionalFonts = [
          "xft:Mononoki:size=12:antialias=true:hinting=true"
        , "xft:Font Awesome 5 Pro-Solid:size=12:weight=bold"
        , "xft:Font Awesome 5 Pro-Regular:size=12"
        , "xft:Font Awesome 5 Brands:size=12"
    ]
    -- , bgColor      = "#282C24"
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

