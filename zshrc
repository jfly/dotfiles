# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

###
### Set prompt (copied and modified from ~/.oh-my-zsh/themes/robbyrussell.zsh-theme)
###
local host=""
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  host=" @%M"
fi
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
export PROMPT='${ret_status}${host} %{$fg[cyan]%}%~%{$reset_color%} $(git_prompt_info)'
#####################

###
### Set up FZF
###
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

###
### Don't add comments starting with space to the history.
###
setopt HIST_IGNORE_SPACE

###
### Bash-like navigation
### Copied from: https://stackoverflow.com/a/10860628/1739415
### Also see https://stackoverflow.com/a/3483679/1739415
###

# Bind ctrl-u to cut to beginning of line.
bindkey "^U" backward-kill-line

# Change behavior of alt-b and alt-f to behave more like bash with regards to
# trailing whitespace.
autoload -Uz forward-word-match
zle -N forward-word forward-word-match
zstyle ':zle:*' skip-whitespace-first true
zstyle ':zle:*' word-chars ''

# Hide files from make autocompletion suggestions.
zstyle ':completion:*:*:make:*' tag-order 'targets'

# Bind alt-backspace to delete one not so aggressive word backwards.
bindkey '^[^?' backward-kill-word

### Bind ctrl-w to delete one aggressive word backwards.
backward-kill-dir() {
    local WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
    zle backward-kill-word
}
zle -N backward-kill-dir
bindkey "^W" backward-kill-dir

#####################

###
### Fix output of time to look more like bash.
### See https://superuser.com/a/71890.
###
export TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'

source ~/.commonrc/commonrc

###
### Set up alt-c to pick a commit
###
fzf-commit-widget() {
  LBUFFER="${LBUFFER}$(pick_commit)"
  local ret=$?
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}
zle     -N    fzf-commit-widget
bindkey '\ec' fzf-commit-widget
###################


# Activate direnv. See https://github.com/direnv/direnv#zsh
if [ "$(hostname)" != "jpi" ]; then
    # Hack to force direnv to reload. See
    # https://github.com/direnv/direnv/blob/a9be1174171b5c0f5472370c41120f770fcff91d/config.go#L133.
    # This is useful when manually resourcing my zshrc. When resourcing my
    # zshrc, my PATH and prompt get clobbered, and it would be really nice for
    # direnv to fix them up. However, direnv notices that the DIRENV_DIFF
    # environment variable is still set, so it decides to do nothing.
    unset DIRENV_DIFF
    unset DIRENV_WATCHES

    eval "$(direnv hook zsh)"
fi
