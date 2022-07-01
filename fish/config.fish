alias vim        'nvim'
alias aisa       'kitty +kitten ssh xkozlov1@aisa.fi.muni.cz'
alias install    'doas emerge --ask'
alias remove     'doas emerge --deselect --ask && doas emerge --depclean'
# alias pacinfo    'paru -Si'
# alias update     'paru -Syu'
alias fixpacman  'doas rm /var/lib/pacman/db.lck'
alias fastreboot 'killall qutebrowser; reboot'
alias tarnow     'tar -acf'
alias untar      'tar -zxvf'
alias mclear     'clear && macchina'
alias ls         'exa'
alias ll         'exa -l'
alias la         'exa -a'
alias lt         'exa -T --icons'
alias dc         'cd'
alias ds         'du -sh * | sort -hr'
alias cat        'bat'
alias less       'bat --style=plain --paging always'
alias sudo       'doas'
alias sudoe      'doasedit'
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

fish_add_path -P $HOME/.local/bin
fish_add_path -P $HOME/.ghcup/bin
fish_add_path -P $HOME/.cargo/bin
fish_add_path -P /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin

source $HOME/.config/fish/fish_prompt.fish

set -g man_underline -i blue
set -g man_bold -o brred
set -g man_standout -b white black

if test -t 0
  and test (tty) = /dev/tty1
  and test ! $DISPLAY
    exec startx > ~/.xorg.log 2>&1
end

if status --is-interactive
    macchina
end

set -q GHCUP_INSTALL_BASE_PREFIX[1]
  or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin /home/martin/.ghcup/bin $PATH # ghcup-env
