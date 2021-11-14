{- import Control.Monad.IO.Class -}
import Data.Functor
import System.Process
import Xmobar


script :: String -> String
script name = "/home/martin/.local/bin/xmobar_scripts/" ++ name

actionWrap :: String -> String -> String
actionWrap cmd str = open ++ str ++ close
  where
    open  = "<action=" ++ cmd ++ ">"
    close = "</action>"

colorWrap :: String -> String -> String
colorWrap fg str = open ++ str ++ close
  where
    open  = "<fc=" ++ fg ++ ">"
    close = "</fc>"

fullWrap :: String -> String -> String -> String
fullWrap fg cmd str = actionWrap cmd $ colorWrap fg str


newtype Bluetooth = Bluetooth String
    deriving (Read, Show)

instance Exec Bluetooth where
    alias (Bluetooth a ) = a
    run   (Bluetooth _ ) = bluetooth

bluetooth :: IO String
bluetooth = readProcess "bluetooth" ["get"] ""
    <&> makeIcon . (=="on") . last . words
  where
    makeIcon isOn = fullWrap (color isOn) cmd icon
    color isOn    = if isOn then "#90A050" else "#F6389D"
    icon          = "<fn=4>\xf293</fn>"
    cmd           = "bluetooth toggle"


data MyVolume = MyVolume String Int
    deriving (Read, Show)

instance Exec MyVolume where
    alias (MyVolume a _) = a
    start (MyVolume _ r) = getVolume r

getVolume :: Int -> (String -> IO ()) -> IO ()
getVolume r callback = readProcess path [] ""
    >>= callback . status . words
    >>  tenthSeconds r
    >>  getVolume r callback
  where
    status [unm, vol] = colorWrap ((color . (=="true")) unm)
        $ (icon . (=="true")) unm ++ vol
    color muted = if muted then "#FF4C6B" else "#90A050"
    icon muted  = if muted then mIcon else unmIcon
    mIcon       = "<fn=2>\xf6a9</fn> "
    unmIcon     = "<fn=2>\xf6a8</fn> "
    path        = script "volume"


data MyBattery = MyBattery String Int
    deriving (Read, Show)

instance Exec MyBattery where
    alias (MyBattery a _) = a
    start (MyBattery _ r) = getBattery r

getBattery :: Int -> (String -> IO ()) -> IO ()
getBattery r callback = mapM
    readFile [capacityPath, statusPath]
    >>= callback . format . map init
    >>  tenthSeconds r
    >>  getBattery r callback
  where
    format [capacity, status] = colorWrap
        color (icon ++ text capacity)
      where
        text capacity = capacity ++ "%"
        (color, icon) = colorIcon capacity status
    colorIcon capacity status
        | status == "Charging" = ("#DDCC00", "<fn=3>\xf376</fn> ")
        | status == "Idle"     = ("#BBBBBB", "<fn=3>\xf376</fn> ")
        | cap    >= 90         = ("#A0DF10", "<fn=3>\xf240</fn> ")
        | cap    >= 65         = ("#CCCC00", "<fn=3>\xf241</fn> ")
        | cap    >= 35         = ("#E58030", "<fn=3>\xf242</fn> ")
        | cap    >= 5          = ("#FF4C6B", "<fn=3>\xf243</fn> ")
        | otherwise            = ("#FF2010", "<fn=3>\xf377</fn> ")
      where
        cap = read capacity::Int
    capacityPath = path ++ "capacity"
    statusPath   = path ++ "status"
    path         = "/sys/class/power_supply/BAT0/"


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

    , Run $ Com (script "pacupdate") [] "pacupdate" 1800

    , Run $ Date "%A, %B %d, %Y  %T" "date" 10

    , Run $ Wireless "wlan0" ["-t", "<ssid> <quality>"] 50

    , Run UnsafeStdinReader
    ]


-- Concatenates two strings with a bar separator in between
(+|+) :: String -> String -> String
str1 +|+ str2 = str1 ++ " " ++ sep ++ " " ++ str2
  where sep = colorWrap "#888888" "<fn=1>|</fn>"


myTemplate :: String
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
  where
    lambda  = fullWrap "#DDDDDD" "rofi -show run"
               "<fn=2>\xf66e</fn> "
    time    = fullWrap "#EEBB30"
                (script "run-process calcurse")
                "%date%"
    wifi    = fullWrap "#EEAA00"
                (script "run-process nmtui")
                "<fn=2>\xf1eb</fn> %wlan0wi%"
    cpu     = fullWrap "#E58030" htop " %cpu%"
    mem     = fullWrap "#FF6050" htop " %memory%"
    bell    = "<fn=2>\xf0f3</fn>"
    volume  = fullWrap "#FF4C6B" mute "%volume%"
    upd     = fullWrap "#C678DD" "st -e yay -Syu" update
    update  = bell ++ "  %pacupdate%"
    battery = "%battery%"
    htop    = script "run-process htop"
    mute    = "pactl set-sink-mute @DEFAULT_SINK@ toggle"


config :: Config
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

