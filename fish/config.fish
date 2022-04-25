alias vim        'nvim'
alias aisa       'kitty +kitten ssh xkozlov1@aisa.fi.muni.cz'
alias install    'yay -S'
alias remove     'yay -Rsn'
alias pacinfo    'yay -Si'
alias update     'yay -Syu'
alias fixpacman  'sudo rm /var/lib/pacman/db.lck'
alias fastreboot 'killall qutebrowser; reboot'
alias qutebrowser '/usr/bin/qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=4'
alias tarnow     'tar -acf'
alias untar      'tar -zxvf'
alias icat       'kitty +kitten icat'
alias kuni       'kitty +kitten unicode_input'
alias kdiff      'kitty +kitten diff'
alias ssh        'kitty +kitten ssh'
alias mclear     'clear && macchina'
alias ls         'ls --color=auto'
alias ds         'du -sh * | sort -hr '
alias ..         'cd ..'
alias ,,         'prevd'
alias ...        'cd ../..'
alias ....       'cd ../../..'

alias refresh-kitty 'killall -s SIGUSR1 kitty'

set fish_greeting

if status --is-interactive
    macchina
end

function mdv
    pandoc $argv[1] | lynx -stdin
end

# eval /home/martin/anaconda3/bin/conda "shell.fish" "hook" $argv | source

source $HOME/.config/fish/ftc.fish info > /dev/null
source $HOME/.config/fish/fish_prompt.fish

fish_add_path -P $HOME/.local/bin
fish_add_path -P $HOME/.ghcup/bin
fish_add_path -P /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin

set -g man_underline -i blue
set -g man_bold -o brred
