alias vim        'nvim'
alias aisa       'kitty +kitten ssh xkozlov1@aisa.fi.muni.cz'
alias install    'paru -S'
alias remove     'paru -Rsn'
alias pacinfo    'paru -Si'
alias update     'paru -Syu'
alias fixpacman  'sudo rm /var/lib/pacman/db.lck'
alias fastreboot 'killall qutebrowser; reboot'
alias qutebrowser '/usr/bin/qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=4'
alias tarnow     'tar -acf'
alias untar      'tar -zxvf'
alias mclear     'clear && macchina'
alias ls         'exa'
alias ll         'exa -l'
alias la         'exa -a'
alias lt         'exa -T --icons'
alias dc         'cd'
alias ds         'du -sh * | sort -hr'
alias cat        'bat --paging never'
alias less       'bat --paging always'
alias ,,         'prevd'
alias ..         'cd ..'
alias ...        'cd ../..'
alias ....       'cd ../../..'

if test $TERM = 'xterm-kitty'
    alias ssh        'kitty +kitten ssh'
    alias icat       'kitty +kitten icat'
    alias kuni       'kitty +kitten unicode_input'
    alias kdiff      'kitty +kitten diff'
    alias refresh-kitty 'killall -s SIGUSR1 kitty'
end

set fish_greeting

if status --is-interactive
    macchina
end

function mdv
    pandoc $argv[1] | lynx -stdin
end

source $HOME/.config/fish/ftc.fish info > /dev/null
source $HOME/.config/fish/fish_prompt.fish

fish_add_path -P $HOME/.local/bin
fish_add_path -P $HOME/.ghcup/bin
fish_add_path -P /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin

set -g man_underline -i blue
set -g man_bold -o brred
set -g man_standout -b white black
