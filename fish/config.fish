alias vim        'nvim'
alias aisa       'kitty +kitten ssh xkozlov1@aisa.fi.muni.cz'
alias install    'yay -S'
alias remove     'yay -Rsn'
alias yeet       'yay -Rsn'
alias pacinfo    'yay -Si'
alias update     'yay -Syu'
alias cleanup    'sudo pacman -Rns (pacman -Qtdq)'
alias fixpacman  'sudo rm /var/lib/pacman/db.lck'
alias fastreboot 'killall qutebrowser; reboot'
alias tarnow     'tar -acf'
alias untar      'tar -zxvf'
alias icat       'kitty +kitten icat'
alias kuni       'kitty +kitten unicode_input'
alias kdiff      'kitty +kitten diff'
alias ssh        'kitty +kitten ssh'
alias mclear     'clear && macchina'
alias ls         'ls --hyperlink=auto --color=auto'
alias ..         'cd ..'
alias ...        'cd ../..'
alias ....       'cd ../../..'

alias refresh-kitty 'kill -s SIGUSR1 (ps ax | awk \'/kitty/ {print $1 " " $5}\' | awk \'/kitty/ {print $1}\')'

set fish_greeting

if status --is-interactive
    macchina
end

function zathura
    tabbed -c -n "Zathura" zathura $argv -e
end

eval /home/martin/anaconda3/bin/conda "shell.fish" "hook" $argv | source

source $HOME/.config/fish/ftc.fish info > /dev/null
source $HOME/.config/fish/fish_prompt.fish

fish_add_path -P $HOME/.local/bin
fish_add_path -P $HOME/.ghcup/bin

conda activate dev
