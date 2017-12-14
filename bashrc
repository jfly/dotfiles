# .bashrc

###
### Setup prompt
###
if tty -s; then
  green=$(tput setaf 2)
  reset=$(tput sgr0)
fi
MY_BODY="\[$green$bold\]\w\[$reset\] @\h"
END=">"
export PS1="${MY_BODY}${END} "
##################################

export HISTCONTROL=ignorespace

# Set up FZF
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Better dircolors for solarized
# http://archlinux.me/w0ng/2012/04/21/better-dircolors-with-solarized/
eval $(dircolors ~/.dir_colors)

# Lower probability of weird line wrapping issues
# https://wiki.archlinux.org/index.php/bash#Line_wrap_on_window_resize
shopt -s checkwinsize

source ~/.commonrc/commonrc
