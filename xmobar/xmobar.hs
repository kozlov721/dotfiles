import Xmobar


myCommands = [
    Run $ Battery
      [ "--template"  , "<leftipat> <acstatus>"
      , "--Low"       , "36"
      , "--High"      , "71"
      , "--low"       , "#A54242"
      , "--normal"    , "#DE935F"
      , "--high"      , "#B5BD68"
      , "--maxtwidth" , "10"
      , "--"
      , "--on-icon-pattern" , "<icon=battery/on/battery_on_%%.xpm/>"
      , "--off-icon-pattern" , "<icon=battery/off/battery_off_%%.xpm/>"
      , "--idle-icon-pattern" , "<icon=battery/idle/battery_idle_%%.xpm/>"
      , "-o" , "<left><fc=#C5C8C6>%</fc> <timeleft>"
      , "-O" , "<left><fc=#C5C8C6>% <timeleft></fc>"
      , "-i" , "<fc=#707880>IDLE</fc>"
    ] 50

    , Run $ Cpu [ "-t"
                , "<fn=2>\xf108</fn>  <total>%"
                , "-H"
                , "50"
                , "--high"
                , "red" ] 20

    , Run $ Memory ["-t", "<fn=2>\xf233</fn>  <usedratio>%"] 20

    , Run $ Com ".local/bin/xmobar/volume" [] "volume" 1

    , Run $ Com ".local/bin/xmobar/pacupdate" [] "pacupdate" 1800

    , Run $ Date "%A, %B %d, %Y  %T" "date" 10

    , Run $ Wireless "wlan0" ["-t", "<fn=2>\xf1eb</fn> <ssid> <quality>"] 10

    , Run UnsafeStdinReader ]


actionWrap :: String -> String -> String
actionWrap cmd str = open ++ str ++ close
    where open  = "<action=" ++ cmd ++ ">"
          close = "</action>"


colorWrap :: String -> String -> String -> String
colorWrap fg bg str = open ++ str ++ close
    where open  = "<fc=" ++ fg ++ "," ++ bg ++ ">"
          close = "</fc>"


fullWrap :: String -> String -> String -> String -> String
fullWrap fg bg cmd str = actionWrap cmd $ colorWrap fg bg str


-- Concatenates two strings with a bar separator in between
(+|+) :: String -> String -> String
str1 +|+ str2 = str1 ++ " " ++ sep ++ " " ++ str2
    where sep = colorWrap "#888888" "" "<fn=1>|</fn>"


myTemplate =  "   "
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
          +|+ battery

    where lambda  = fullWrap "#DDDDDD" "" "`rofi -show run`" "<fn=2>\xf66e</fn>  "
          time    = fullWrap "#DDBB20" "" "`.local/bin/xmobar/run-process calcurse`" "%date%"
          wifi    = fullWrap "#EEAA00" "" "`.local/bin/xmobar/run-process nmtui`" "%wlan0wi%"
          cpu     = fullWrap "#E58030" "" htop "%cpu%"
          mem     = fullWrap "#FF6050" "" htop " %memory%"
          bell    = "<fn=2>\xf0f3</fn>"
          volume  = fullWrap "#FF4C6B" "" mute "<fn=2>\xf6a8</fn>  %volume%"
          upd     = fullWrap "#C678DD" "" "`st -e yay -Syu`" update
          update  = bell ++ "  %pacupdate%"
          battery = "%battery%"
          htop    = "`.local/bin/xmobar/run-process htop`"
          mute    = "`pactl set-sink-mute @DEFAULT_SINK@ toggle`"
          textColor = "#FFFFFF"


config = defaultConfig {
      font="xft:Roboto:size=11:weight=semibold:hinting=true"
    , additionalFonts = [
          "xft:Mononoki:size=11:antialias=true:hinting=true"
        , "xft:Font Awesome 5 Pro-Regular:size=11:weight=bold"
        , "xft:Font Awesome 5 Pro-Regular:size=11"
    ]
    , bgColor      = "#282C34"
    , fgColor      = "#FF6C6B"
    , alpha        = 250
    , position     = Top
    , lowerOnStart = True
    , hideOnStart  = False
    , persistent   = True
    , iconRoot     = ".config/xmobar/icons"
    , sepChar      = "%"
    , alignSep     = "}{"
    , commands     = myCommands
    , template     = myTemplate
}


main :: IO ()
main = xmobar config

