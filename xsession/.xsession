#!/bin/sh

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK
export PATH="$HOME/.local/bin:$PATH"
<<<<<<< HEAD
export PATH="$HOME/.ghcup/bin:$PATH"

xsetroot -cursor_name left_ptr
bluetooth on
=======
# export PATH="$HOME/.cabal/bin:$PATH"
# export PATH="$HOME/.ghcup/bin:$PATH"
>>>>>>> 9897914bd1c97c7bbc8b5693107a2bf40027ed0b
xset r rate 300 40
setxkbmap -layout cz coder
xinput set-prop 'ELAN2602:00 04F3:3109 Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'ELAN2602:00 04F3:3109 Touchpad' 'libinput Accel Speed' 0.4
nitrogen --restore &
dropbox &
bluetooth off
