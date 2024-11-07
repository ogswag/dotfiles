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
    # Completion style options
    local glob_case_sensitivity completion_case_sensitivity
    zstyle -s ':zim:glob' case-sensitivity glob_case_sensitivity || glob_case_sensitivity=insensitive
    zstyle -s ':zim:completion' case-sensitivity completion_case_sensitivity || completion_case_sensitivity=insensitive

    # Move cursor to end of word if a full completion is inserted.
    setopt ALWAYS_TO_END

    if [[ ${glob_case_sensitivity} == sensitive ]]; then
      setopt CASE_GLOB
    else
      setopt NO_CASE_GLOB
    fi

    # Don't beep on ambiguous completions.
    setopt NO_LIST_BEEP

    #
    # Completion module options
    #

    # Enable caching
    zstyle ':completion::complete:*' use-cache on

    # Group matches and describe.
    zstyle ':completion:*:*:*:*:*' menu select
    zstyle ':completion:*:matches' group yes
    zstyle ':completion:*:options' description yes
    zstyle ':completion:*:options' auto-description '%d'
    zstyle ':completion:*:corrections' format '%F{green}-- %d (errors: %e) --%f'
    zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
    zstyle ':completion:*:messages' format '%F{purple}-- %d --%f'
    zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
    zstyle ':completion:*' format '%F{yellow}-- %d --%f'
    zstyle ':completion:*' group-name ''
    zstyle ':completion:*' verbose yes
    if [[ ${completion_case_sensitivity} == sensitive ]]; then
      zstyle ':completion:*' matcher-list '' 'r:|?=**'
    else
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
    fi

    # Insert a TAB character instead of performing completion when left buffer is empty.
    zstyle ':completion:*' insert-tab false

    # Ignore useless commands and functions
    zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec)|prompt_*)'
    # Array completion element sorting.
    zstyle ':completion:*:*:-subscript-:*' tag-order 'indexes' 'parameters'

    # Directories
    if (( ${+LS_COLORS} )); then
      zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
    else
      # Use same LS_COLORS definition from utility module, in case it was not set
      zstyle ':completion:*:default' list-colors ${(s.:.):-di=1;34:ln=35:so=32:pi=33:ex=31:bd=1;36:cd=1;33:su=30;41:sg=30;46:tw=30;42:ow=30;43}
    fi
    zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
    zstyle ':completion:*' squeeze-slashes true

    # History
    zstyle ':completion:*:history-words' stop yes
    zstyle ':completion:*:history-words' remove-all-dups yes
    zstyle ':completion:*:history-words' list false
    zstyle ':completion:*:history-words' menu yes

    # Populate hostname completion.
    zstyle -e ':completion:*:hosts' hosts 'reply=(
    ${=${=${=${${(f)"$(cat {/etc/ssh/ssh_,~/.ssh/}known_hosts{,2} 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
    ${=${(f)"$(cat /etc/hosts 2>/dev/null; (( ${+commands[ypcat]} )) && ypcat hosts 2>/dev/null)"}%%(\#)*}
      ${=${${${${(@M)${(f)"$(cat ~/.ssh/config{,.d/*(N)} 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
      )'

    # Don't complete uninteresting users...
    zstyle ':completion:*:*:*:users' ignored-patterns \
      '_*' adm amanda apache avahi beaglidx bin cacti canna clamav daemon dbus \
      distcache dovecot fax ftp games gdm gkrellmd gopher hacluster haldaemon \
      halt hsqldb ident junkbust ldap lp mail mailman mailnull mldonkey mysql \
      nagios named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
      operator pcap postfix postgres privoxy pulse pvm quagga radvd rpc rpcuser \
      rpm shutdown squid sshd sync uucp vcsa xfs

    # ... unless we really want to.
    zstyle ':completion:*' single-ignored show

    # Ignore multiple entries.
    zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
    zstyle ':completion:*:rm:*' file-patterns '*:all-files'

    # Man
    zstyle ':completion:*:manuals' separate-sections true
    zstyle ':completion:*:manuals.(^1*)' insert-sections true

    unset glob_case_sensitivity completion_case_sensitivity

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
