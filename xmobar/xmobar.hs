import Control.Monad.IO.Class
import Data.Functor
import System.Process
import Xmobar


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


data MyVolume = MyVolume String Int
    deriving (Read, Show)

instance Exec MyVolume where
    alias (MyVolume a _) = a
    start (MyVolume _ r) = getVolume r

newtype Bluetooth = Bluetooth String
    deriving (Read, Show)

instance Exec Bluetooth where
    alias (Bluetooth a ) = a
    run (Bluetooth _ ) = bluetooth


bluetooth :: IO String
bluetooth = readProcess "bluetooth" ["get"] "" <&>
    icon . (=="on") . last . words
        where icon isOn = fullWrap (color isOn) "" toggle i
              color isOn = if isOn then "#90A050" else "#F6389D"
              i = "<fn=4>\xf293</fn>"
              toggle = "`bluetooth toggle`"


getVolume :: Int -> (String -> IO ()) -> IO ()
getVolume r callback = readProcess path [] "" >>=
            callback . status . words >>
            tenthSeconds r >> getVolume r callback

        where status [unm, vol] =
                  colorWrap ((color . (=="true")) unm) ""
                $ (icon . (=="true")) unm ++ vol
              color unmuted = if unmuted then "#90A050" else "#FF4C6B"
              icon unmuted  = if unmuted then unmIcon else mIcon
              mIcon         = "<fn=2>\xf6a9</fn> "
              unmIcon       = "<fn=2>\xf6a8</fn> "
              path          = "/home/martin/.local/bin/xmobar/volume"


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

    , Run $ Memory ["-t", "<fn=3>\xf538</fn>  <usedratio>%"] 20

    , Run $ MyVolume "volume" 1

    , Run $ Bluetooth "bluetooth"

    , Run $ Com ".local/bin/xmobar/pacupdate" [] "pacupdate" 1800

    , Run $ Date "%A, %B %d, %Y  %T" "date" 10

    , Run $ Wireless "wlan0" ["-t", "<fn=2>\xf1eb</fn> <ssid> <quality>"] 10

    , Run UnsafeStdinReader ]


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
          +|+  "%bluetooth%"
          +|+ battery
          ++  " "

    where lambda  = fullWrap "#DDDDDD" "" "`rofi -show run`"
                    "<fn=2>\xf66e</fn>  "
          time    = fullWrap "#DDBB20" "" 
                    "`.local/bin/xmobar/run-process calcurse`"
                    "%date%"
          wifi    = fullWrap "#EEAA00" "" "`.local/bin/xmobar/run-process nmtui`" "%wlan0wi%"
          cpu     = fullWrap "#E58030" "" htop " %cpu%"
          mem     = fullWrap "#FF6050" "" htop " %memory%"
          bell    = "<fn=2>\xf0f3</fn>"
          volume  = fullWrap "#FF4C6B" "" mute "%volume%"
          upd     = fullWrap "#C678DD" "" "`st -e yay -Syu`" update
          update  = bell ++ "  %pacupdate%"
          battery = "%battery%"
          htop    = "`.local/bin/xmobar/run-process htop`"
          mute    = "`pactl set-sink-mute @DEFAULT_SINK@ toggle`"
          textColor = "#FFFFFF"


config = defaultConfig {
      font="xft:Roboto:size=12:weight=semibold:hinting=true"
    , additionalFonts = [
          "xft:Mononoki:size=12:antialias=true:hinting=true"
        , "xft:Font Awesome 5 Pro-Solid:size=12:weight=bold"
        , "xft:Font Awesome 5 Pro-Regular:size=12"
        , "xft:Font Awesome 5 Brands:size=12"
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

