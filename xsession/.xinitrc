export PATH="$HOME/.local/bin:$PATH"
xset r rate 300 40
setxkbmap -layout cz coder
xrdb -load ~/.Xresources &
nitrogen --restore &

xinput set-prop                      \
    'ELAN2602:00 04F3:3109 Touchpad' \
    'libinput Natural Scrolling Enabled' 1
xinput set-prop                      \
    'ELAN2602:00 04F3:3109 Touchpad' \
    'libinput Tapping Enabled' 1
xinput set-prop                      \
    'ELAN2602:00 04F3:3109 Touchpad' \
    'libinput Accel Speed' 0.5
bluetoothctl power off
exec /home/martin/.cache/xmonad/xmonad-x86_64-linux
