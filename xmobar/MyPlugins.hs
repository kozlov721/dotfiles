module MyPlugins where

import Data.Functor
import System.Process
import Xmobar

---------------- Helper functions -------------------

script :: String -> String
script name = "~/.local/bin/xmobar_scripts/" ++ name

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

------------------ Bluetooth -------------------------

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

------------------ Volume ---------------------------

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

------------------ Battery --------------------------

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
        | cap    >= 90         = ("#B5DF10", "<fn=3>\xf240</fn> ")
        | cap    >= 65         = ("#CDCD00", "<fn=3>\xf241</fn> ")
        | cap    >= 35         = ("#E58030", "<fn=3>\xf242</fn> ")
        | cap    >= 5          = ("#FF4C6B", "<fn=3>\xf243</fn> ")
        | otherwise            = ("#FF2010", "<fn=3>\xf377</fn> ")
      where
        cap = read capacity::Int
    capacityPath = path ++ "capacity"
    statusPath   = path ++ "status"
    path         = "/sys/class/power_supply/BAT0/"

--------------- WiFi ------------------------

data MyWiFi = MyWiFi String Int
    deriving (Read, Show)

instance Exec MyWiFi where
    alias (MyWiFi a _) = a
    start (MyWiFi _ r) = getWiFi r


getWiFi :: Int -> (String -> IO ()) -> IO ()
getWiFi r callback = return ()

getQuality :: Double -> Integer
getQuality = round . (/ 0.7) . (+ 110) . clamp (-110) (-40)
  where
    clamp l r v
      | v < l = l
      | v > r = r
      | otherwise = v
