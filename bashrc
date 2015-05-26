# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.aliases

export VISUAL=vim
export EDITOR=vim

if tty -s; then
  green=$(tput setaf 2)
  reset=$(tput sgr0)
fi
MY_BODY="\[$green$bold\]\w\[$reset\] @\h"
END=">"
export PS1="${MY_BODY}${END} "

export PATH=$PATH:$HOME/bin

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

export PATH=$PATH:$HOME/.gem/ruby/2.2.0/bin
export PATH=$PATH:/usr/local/heroku/bin

# ctrl-shift-n for vte
[[ -f /etc/profile.d/vte.sh ]] && source /etc/profile.d/vte.sh

# Better dircolors for solarized
# http://archlinux.me/w0ng/2012/04/21/better-dircolors-with-solarized/
eval $(dircolors ~/.dir_colors)

# Lower probability of weird line wrapping issues
# https://wiki.archlinux.org/index.php/bash#Line_wrap_on_window_resize
shopt -s checkwinsize

[[ -f ~/.bashrc_local ]] && source ~/.bashrc_local

# startx at login
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
