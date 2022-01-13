module MyPlugins where

import qualified Data.Bifunctor as Bi
import           System.Process
import           Xmobar

---------------- Helper functions -------------------

space = "<fn=4> </fn>" :: String
doubleSpace = space ++ space :: String

-- A little bit of custom operators hell, enjoy.

-- | Concatenates two strings with a space between
(<->) :: String -> String -> String
str1 <-> str2 = str1 ++ space ++ str2

-- | Concatenates two strings with two spaces in between
(<-->) :: String -> String -> String
str1 <--> str2 = str1 ++ doubleSpace ++ str2

-- | Concatenates two strings with a bar separator in between
(<|>) :: String -> String -> String
str1 <|> str2 = str1 <-> sep <-> str2
  where sep = colorWrap "#888888" "<fn=0>|</fn>"

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

------------------ Bluetooth -------------------------

newtype Bluetooth = Bluetooth String
    deriving (Read, Show)

instance Exec Bluetooth where
    alias (Bluetooth a ) = a
    run   (Bluetooth _ ) = bluetooth

bluetooth :: IO String
bluetooth = makeIcon
    . (=="on")
    . (!!2)
    . words
    <$> readProcess "bluetooth" ["get"] ""
  where
    makeIcon isOn = fullWrap (color isOn) cmd icon
    color isOn    = if isOn then "#90A050" else "#F6389D"
    icon          = "<fn=3>\xf293</fn>"
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
    mute   <- (=="true") . init . snd'
        <$> readProcessWithExitCode "pamixer" ["--get-mute"] ""
    callback $ status mute volume
    tenthSeconds r
    getVolume r callback
  where
    snd' (_, x, _) = x
    status unm vol = colorWrap (color unm)
        $ icon unm <-> vol
    color muted = if muted then "#FF4C6B" else "#90A050"
    icon muted  = if muted then mIcon else unmIcon
    mIcon       = "<fn=1>\xf6a9</fn>"
    unmIcon     = "<fn=1>\xf6a8</fn>"

------------------ Battery --------------------------

data MyBattery = MyBattery String Int
    deriving (Read, Show)

instance Exec MyBattery where
    alias (MyBattery a _) = a
    start (MyBattery _ r) = getBattery r

getBattery :: Int -> (String -> IO ()) -> IO ()
getBattery r callback = mapM readFile
    [capacityPath, statusPath]
    >>= callback . format . map init
    >>  tenthSeconds r
    >>  getBattery r callback
  where
    format [capacity, status] = colorWrap
        color (icon ++ capacity ++ "%")
      where
        (color, icon) = Bi.second (++space)
            $ colorIcon (read capacity :: Int) status
    colorIcon cap status
        | status == "Full"     = ("#BBBBBB", "<fn=2>\xf376</fn>")
        | status == "Charging" = ("#DDCC00", "<fn=2>\xf376</fn>")
        | cap    >= 90         = ("#B5DF10", "<fn=2>\xf240</fn>")
        | cap    >= 65         = ("#CDCD00", "<fn=2>\xf241</fn>")
        | cap    >= 35         = ("#E58030", "<fn=2>\xf242</fn>")
        | cap    >= 5          = ("#FF4C6B", "<fn=2>\xf243</fn>")
        | otherwise            = ("#FF2010", "<fn=2>\xf377</fn>")
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
    >>= callback . status . length . lines . snd'
    >>  tenthSeconds r
    >>  getPacmanUpdates r callback
  where
    status :: Int -> String
    status updates = uncurry colorWrap
        $ Bi.second (<->show updates) $ iconColor updates
    iconColor updates
        | updates < 20  = ("#C678DD", "<fn=1>\xf0f3</fn>")
        | updates < 100 = ("#FF38BB", "<fn=1>\xf8fa</fn>")
        | otherwise    = ("#FF2010", "<fn=1>\xf848</fn>")
    snd' (_, x, _) = x

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
    clamp l r v = max l $ min r v
