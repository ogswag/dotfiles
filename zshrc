export PS1="%n@%m %F{red}%1~%f %% "

# Enable completion
autoload -Uz compinit && compinit

# Color file completions
zstyle ':completion:*' list-colors ''

# Group completions by type
zstyle ':completion:*' group-name ''
zstyle ':completion:*' format '%B%d%b'

# History file for zsh
HISTFILE=~/.zsh_history

# How many commands to store in history
HISTSIZE=1000
SAVEHIST=$HISTSIZE

# Ignore a duplicate of the previous command
setopt HIST_IGNORE_DUPS

# Append to history file
setopt APPEND_HISTORY

# Save history immediately after each command
setopt INC_APPEND_HISTORY

# Share history between active zsh sessions
setopt share_history

# Disable beeping
setopt NO_BEEP

# Recognize comments on the command line
setopt INTERACTIVE_COMMENTS

# Emacs keybindings
bindkey -e

# Error on a redirections which would overwrite an existing file
setopt NO_CLOBBER

# Disable multios
setopt NO_MULTIOS

# Disable flow control
setopt NO_FLOW_CONTROL

# alias l="eza --group-directories-first -lhU --time-style='long-iso'"
alias l="ls -FAG"
alias n="nvim"
alias nv="nvim"
alias v="vim"

export VISUAL=e

export PATH="/usr/local/opt/jpeg/bin:$PATH"

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
