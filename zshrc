export PS1="%n@%m %F{red}%1~%f %% "

##? Clone a plugin, identify its init file, source it, and add it to your fpath.
function plugin-load {
  local repo plugdir initfile initfiles=()
  : ${ZPLUGINDIR:=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins}
  for repo in $@; do
    plugdir=$ZPLUGINDIR/${repo:t}
    initfile=$plugdir/${repo:t}.plugin.zsh
    if [[ ! -d $plugdir ]]; then
      echo "Cloning $repo..."
      git clone -q --depth 1 --recursive --shallow-submodules \
        https://github.com/$repo $plugdir
    fi
    if [[ ! -e $initfile ]]; then
      initfiles=($plugdir/*.{plugin.zsh,zsh-theme,zsh,sh}(N))
      (( $#initfiles )) || { echo >&2 "No init file '$repo'." && continue }
      ln -sf $initfiles[1] $initfile
    fi
    fpath+=$plugdir
    (( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
  done
}

# where do you want to store your plugins?
ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/.zsh_plugins}

# get zsh_unplugged and store it with your other plugins
if [[ ! -d $ZPLUGINDIR/zsh_unplugged ]]; then
  git clone --quiet https://github.com/mattmc3/zsh_unplugged $ZPLUGINDIR/zsh_unplugged
fi
source $ZPLUGINDIR/zsh_unplugged/zsh_unplugged.zsh

# make list of the Zsh plugins you use
repos=(
  zdharma-continuum/fast-syntax-highlighting
  zsh-users/zsh-history-substring-search
)

# now load your plugins
plugin-load $repos

# zsh-history-substring-search configuration
bindkey '^[[A' history-substring-search-up # or '\eOA'
bindkey '^[[B' history-substring-search-down # or '\eOB'
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1

# Enable completion
autoload -Uz compinit && compinit

# Emacs keybindings
bindkey -e

# Color file completions
zstyle ':completion:*' list-colors ''

# Group completions by type
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' format '%B%d%b'

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

# Error on a redirections which would overwrite an existing file
setopt NO_CLOBBER

# Disable multios
setopt NO_MULTIOS

# Disable flow control
setopt NO_FLOW_CONTROL

# alias l="eza --group-directories-first -lhU --time-style='long-iso'"
alias l="eza --sort=type -l --hyperlink"
alias n="nvim"
alias nv="nvim"
alias v="vim"
alias cd-focus-config="cd ~/Library/Application\ Support/dev.focus-editor"

export PATH="/usr/local/opt/jpeg/bin:$PATH"

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/bin:$PATH"

mdcd () {
    mkdir -p "$1";
    cd "$1"
}
export PATH="$(brew --prefix)/bin:$PATH"

eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
