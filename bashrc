# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
export VISUAL=vim
export EDITOR=vim

green=$(tput setaf 2)
reset=$(tput sgr0)
MY_BODY="\[$green$bold\]\w\[$reset\] @\h"
END=">"

export PS1="${MY_BODY}${END} "

alias bs="mosh bs336 -- bash -cli bs"
