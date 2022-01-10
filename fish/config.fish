alias vim 'nvim'
alias aisa 'kitty +kitten ssh xkozlov1@aisa.fi.muni.cz'
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
alias ssh="kitty +kitten ssh"
alias refresh-kitty 'kill -s SIGUSR1 (ps ax | awk \'/kitty/ {print $1 " " $5}\' | awk \'/kitty/ {print $1}\')'
alias mclear 'clear && macchina'
alias ls 'ls --hyperlink=auto --color=auto'
# alias vimb 'tabbed -c vimb -e'
alias .. 'cd ..'
alias ... 'cd ../..'
alias .... 'cd ../../..'

set fish_greeting

if status --is-interactive
    macchina
end

function help
    $argv --help | less
end

function zathura
    tabbed -c -n "Zathura" zathura $argv -e
end

eval /home/martin/anaconda3/bin/conda "shell.fish" "hook" $argv | source

source /usr/share/doc/find-the-command/ftc.fish info > /dev/null

fish_add_path -P $HOME/.local/bin
fish_add_path -P $HOME/.ghcup/bin
fish_add_path -P $HOME/.cabal/bin
conda activate dev
