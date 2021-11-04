import Xmobar


myCommands = [
    Run $ Battery
      [ "--template"  , "<leftipat> <acstatus>"
      , "--Low"       , "36"
      , "--High"      , "71"
      , "--low"       , "#a54242"
      , "--normal"    , "#de935f"
      , "--high"      , "#b5bd68"
      , "--maxtwidth" , "10"
      , "--"
      , "--on-icon-pattern" , "<icon=battery/on/battery_on_%%.xpm/>"
      , "--off-icon-pattern" , "<icon=battery/off/battery_off_%%.xpm/>"
      , "--idle-icon-pattern" , "<icon=battery/idle/battery_idle_%%.xpm/>"
      , "-o" , "<left><fc=#c5c8c6>%</fc> <timeleft>"
      , "-O" , "<left><fc=#c5c8c6>% <timeleft></fc>"
      , "-i" , "<fc=#707880>IDLE</fc>"
    ] 50

    -- Cpu usage in percent
    , Run $ Cpu [ "-t"
                , "<fn=2>\xf108</fn>  cpu: (<total>%)"
                , "-H"
                , "50"
                , "--high"
                , "red" ] 20

    -- Ram usage in percent
    , Run $ Memory ["-t", "<fn=2>\xf233</fn>  mem: (<usedratio>%)"] 20

    -- Current volume
    , Run $ Com ".local/bin/xmobar/volume" [] "volume" 1

    -- Check for pacman updates
    , Run $ Com ".local/bin/xmobar/pacupdate" [] "pacupdate" 1800

    -- Time and date
    , Run $ Date "%A, %B %d, %Y  %T" "date" 10

    -- Prints out the workspaces and layout
    , Run $ UnsafeStdinReader ]


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


-- Concatenates two string with bar separator in between
(+|+) :: String -> String -> String
str1 +|+ str2 = str1 ++ " " ++ sep ++ " " ++ str2
    where sep = colorWrap "#888888" "" "<fn=1>|</fn>"


myTemplate =  "   "
          ++  lambda
          ++  " %UnsafeStdinReader% "
          ++  "}"
          ++  time
          ++  "{"
          ++  cpu
          +|+ mem
          +|+ upd
          +|+ volume
          +|+ battery

    where lambda  = fullWrap "#ffffff" "" "`dmenu_run`" "λ"
          time    = fullWrap "#ddbb20" "" "`st -e calcurse`" "%date%"
          cpu     = fullWrap "#ecbe7b" "" htop "%cpu%"
          mem     = fullWrap "#ff6c6b" "" htop " %memory%"
          upd     = fullWrap "#c678dd" "" "`st -e yay -Syu`" update
          update  = bell ++ " %pacupdate%"
          bell    = "<fn=2>\xf0f3</fn>"
          volume  = fullWrap "#ff6c6b" "" mute "%volume%"
          battery = "%battery%"
          htop    = "`.local/bin/xmobar/htop`"
          mute    = "`pactl set-sink-mute @DEFAULT_SINK@ toggle`"


config = defaultConfig {
      font="xft:Roboto:size=11:weight=semibold:hinting=true"
    , additionalFonts = [
          "xft:Mononoki:size=11:antialias=true:hinting=true"
        , "xft:Font Awesome 5 Free Solid:size=11"
        , "xft:Font Awesome 5 Brands:size=11"
    ]
    , bgColor      = "#282c34"
    , fgColor      = "#ff6c6b"
    , alpha        = 250
    , position     = Top
    , lowerOnStart = True
    , hideOnStart  = False
    , persistent   = True
    , iconRoot     = ".config/xmobar/icons"
    , sepChar      = "%"
    , commands     = myCommands
    , alignSep     = "}{"
    , template     = myTemplate
}


main :: IO ()
main = xmobar config

