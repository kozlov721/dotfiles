alias vim 'nvim'
alias aisa 'ssh xkozlov1@aisa.fi.muni.cz'
alias install 'doas -- pacman -S'
alias remove 'doas -- pacman -Rsn'
alias update 'doas -- pacman -Syu'
alias full_update 'doas -- pacman -Syu && yay -Syu'
alias cleanup 'doas -- pacman -Rns (pacman -Qtdq)'
alias fixpacman 'doas -- rm /var/lib/pacman/db.lck'
alias tarnow 'tar -acf'
alias untar 'tar -zxvf'
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
