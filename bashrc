# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.aliases

export VISUAL=vim
export EDITOR=vim

green=$(tput setaf 2)
reset=$(tput sgr0)
MY_BODY="\[$green$bold\]\w\[$reset\] @\h"
END=">"

export PS1="${MY_BODY}${END} "

export PATH=$PATH:$HOME/bin

# ctrl-shift-n for vte
source /etc/profile.d/vte.sh

# Better dircolors for solarized
# http://archlinux.me/w0ng/2012/04/21/better-dircolors-with-solarized/
eval $(dircolors ~/.dir_colors)

[[ -f ~/.bashrc_local ]] && source ~/.bashrc_local

# startx at login
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec ssh-agent startx
