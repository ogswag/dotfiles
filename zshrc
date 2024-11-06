# Get the operating system name
OS=$(uname)

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ Plugins Initialization                                                   │
# └──────────────────────────────────────────────────────────────────────────┘

# where should we download your Zsh plugins?
#ZPLUGINDIR=$ZDOTDIR/.zsh_plugins

##? Clone a plugin, identify its init file, source it, and add it to your fpath.
function plugin-load {
local repo plugdir initfile initfiles=()
: ${ZPLUGINDIR:=${ZDOTDIR:-~/.config/zsh}/plugins}
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
    (( $#initfiles )) || { echo >&2 "No init file found '$repo'." && continue }
    ln -sf $initfiles[1] $initfile
  fi
  fpath+=$plugdir
  (( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
done
}

# make a github repo plugins list
plugins=(
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-history-substring-search
  zsh-users/zsh-completions
  ael-code/zsh-colored-man-pages
  # load these at hypersonic load speeds with zsh-defer
  romkatv/zsh-defer
  olets/zsh-abbr
  Skylor-Tang/auto-venv
)
plugin-load $plugins

# history setup
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

# Bind history substring keys 1) up and down; 2) Ctrl-P and Ctrl-N
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# Check for homebrew existence
if [[ "$OS" == "Darwin" ]]; then
    if ! command -v "brew" > /dev/null 2>&1; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
fi

# Setup completions
if which compinit > /dev/null 2>&1; then
    brew install zsh-completions
fi

if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

    autoload -Uz compinit
    compinit
fi

if ! command -v "carapace" > /dev/null 2>&1; then
    brew install carapace
fi

if command -v "carapace" > /dev/null 2>&1; then
    export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
    zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
    source <(carapace _carapace)
else
fi

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ Personal config                                                          │
# └──────────────────────────────────────────────────────────────────────────┘
export PS1="%n@%m %F{red}%1~%f %% "

GPG_TTY=$(tty)
export GPG_TTY

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="$HOME/Workspace/scripts:$PATH"

# Different aliases for ls on different systems
if [[ "$OS" == "Linux" ]]; then
    alias l="ls --group-directories-first -FAlh --color=auto"
elif [[ "$OS" == "Darwin" ]]; then
    if ! command -v "eza" > /dev/null 2>&1; then
        brew install eza
    fi
    if command -v "eza" > /dev/null 2>&1; then
        alias l="eza --group-directories-first -lhU --icons --git --time-style='long-iso'"
    fi
fi

# Different aliases for (neo)vim
# if command -v "nvim" > /dev/null 2>&1; then
#     alias v="nvim"
# else
#     alias v="vim"
# fi

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ Custom Functions                                                         │
# └──────────────────────────────────────────────────────────────────────────┘
mkcd(){
  mkdir $1;
  cd $1;
}

up(){
  if [[ "$#" -ne 1 ]]; then
    cd ..
  elif ! [[ $1 =~ '^[0-9]+$' ]]; then
    echo "Error: up should be called with the number of directories to go up. The default is 1."
  else
    local d=""
    limit=$1
    for ((i=1 ; i <= limit ; i++))
    do
      d=$d/..
    done
    d=$(echo $d | sed 's/^\///')
    cd $d
  fi
}
