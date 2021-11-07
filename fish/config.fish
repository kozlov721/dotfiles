cat ~/.cache/wal/sequences &

alias vim 'nvim'
alias aisa 'ssh xkozlov1@aisa.fi.muni.cz'
alias install 'yay -S'
alias remove 'yay -Rsn'
alias yeet 'yay -Rsn'
alias pacinfo 'yay -Si'
alias update 'yay -Syu'
alias cleanup 'sudo pacman -Rns (pacman -Qtdq)'
alias fixpacman 'sudo rm /var/lib/pacman/db.lck'
alias tarnow 'tar -acf'
alias untar 'tar -zxvf'
alias icat 'kitty +kitten icat'
alias kuni 'kitty +kitten unicode_input'
alias kdiff 'kitty +kitten diff'
alias refresh-kitty 'kill -s SIGUSR1 (ps ax | grep \'kitty\($\| \)\' | awk \'{print $1}\')'
alias .. 'cd ..'
alias ... 'cd ../..'
alias .... 'cd ../../..'

set fish_greeting

if status --is-interactive
    neofetch
end

function zathura
    tabbed -c -n "Zathura" zathura $argv -e
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/martin/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

source /usr/share/doc/find-the-command/ftc.fish


