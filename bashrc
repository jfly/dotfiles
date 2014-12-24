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

# ctrl-shift-n for gnome-terminal
source /etc/profile.d/vte.sh

[[ -f ~/.bashrc_local ]] && source ~/.bashrc_local

# startx at login
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec ssh-agent startx
