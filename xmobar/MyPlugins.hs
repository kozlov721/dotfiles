module MyPlugins where

import System.Process
import Xmobar

---------------- Helper functions -------------------

script :: String -> String
script = (++) "/home/martin/.local/bin/xmobar_scripts/"

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
fullWrap fg cmd = actionWrap cmd . colorWrap fg

------------------ Bluetooth -------------------------

newtype Bluetooth = Bluetooth String
    deriving (Read, Show)

instance Exec Bluetooth where
    alias (Bluetooth a ) = a
    run   (Bluetooth _ ) = bluetooth

bluetooth :: IO String
bluetooth = makeIcon . (=="on") . last . words
    <$> readProcess "bluetooth" ["get"] ""
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
getVolume r callback = do
    volume <- init . snd'
        <$> readProcessWithExitCode "pamixer" ["--get-volume"] ""
    mute <- (=="true") . init . snd'
        <$> readProcessWithExitCode "pamixer" ["--get-mute"] ""
    callback $ status mute volume
    tenthSeconds r
    getVolume r callback
  where
    snd' (_, x, _) = x
    status unm vol = colorWrap (color unm)
        $ icon unm (read vol::Int) ++ vol
    color muted     = if muted then "#FF4C6B" else "#90A050"
    icon muted vol  = if muted then mIcon else unmIcon vol
    unmIcon vol
        | vol < 5   = "<fn=2>\xf026</fn> "
        | vol < 30  = "<fn=2>\xf027</fn> "
        | vol < 75  = "<fn=2>\xf6a8</fn> "
        | otherwise = "<fn=2>\xf028</fn> "
    mIcon           = "<fn=2>\xf6a9</fn> "
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
        | status == "Full"     = ("#BBBBBB", "<fn=3>\xf376</fn> ")
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

------------------ Pacman Updates ----------------------

data Pacman = Pacman String Int
    deriving (Read, Show)

instance Exec Pacman where
    alias (Pacman a _) = a
    start (Pacman _ r) = getPacmanUpdates r

getPacmanUpdates :: Int -> (String -> IO ()) -> IO ()
getPacmanUpdates r callback =
    readProcessWithExitCode "checkupdates" [] ""
    >>= callback . status . length . filter (=='\n') . snd'
    >>  tenthSeconds r
    >>  getPacmanUpdates r callback
  where
    status number = uncurry colorWrap $ iconColor number
    iconColor number
        | number < 20  = ("#C678DD", format "<fn=2>\xf0f3</fn> ")
        | number < 100 = ("#FF38BB", format "<fn=2>\xf8fa</fn> ")
        | otherwise    = ("#FF2010", format "<fn=3>\xf848</fn> ")
      where
        format = (++ show number)
    snd' (_, x, _) = x

