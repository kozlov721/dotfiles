#!/bin/sh

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"

bluetooth on
xset r rate 300 40
setxkbmap -layout cz coder
nitrogen --restore &
dropbox &

imwheel -kill
