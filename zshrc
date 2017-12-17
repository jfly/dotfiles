# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/jeremy/.oh-my-zsh

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
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
PROMPT='${ret_status} %{$fg[cyan]%}%~%{$reset_color%} $(git_prompt_info)'
#####################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

source ~/.commonrc/commonrc
