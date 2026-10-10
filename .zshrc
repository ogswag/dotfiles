os=$(uname)
host=$(hostname -s)

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# If not running interactively, don't do anything
[[ -o interactive ]] || return

if [[ $os == Darwin && -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if (( $+commands[mise] )); then
  eval "$(mise activate zsh)"
fi

if [ -f ~/.zsh/zsh_plugins ]; then
    source ~/.zsh/zsh_plugins
fi
if [ -f ~/.zsh/zsh_completion ]; then
    source ~/.zsh/zsh_completion
fi

if [ -f ~/.zsh/zsh_aliases ]; then
    source ~/.zsh/zsh_aliases
fi

if [ -f ~/.zsh/zsh_prompt ]; then
    source ~/.zsh/zsh_prompt
fi


if [[ $os == "Linux" ]]; then
    export EDITOR='vim'
    export SUDO_EDITOR='vim'
    export VISUAL='vim'
elif [[ $os == "Darwin" ]]; then
    export EDITOR='vim'
    export SUDO_EDITOR='vim'
    export VISUAL='vim'
fi

mkcd () {
    mkdir -p "$1";
    cd "$1"
}

# Prevent duplicate PATH entries on re-source
typeset -U PATH path
if [[ $os == "Linux" ]]; then
    export LD_LIBRARY_PATH=/usr/local/lib
elif [[ $os != "Darwin" ]]; then
    echo "Unknown operating system."
fi

# ===== History Configuration =====
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000

setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data
setopt hist_find_no_dups      # don't show duplicates when searching history

# ===== Keybindings =====
bindkey '^\' redo

# ===== Other Options =====
setopt no_beep                  # disable beeping
setopt interactive_comments     # allow comments in interactive shells
setopt auto_cd                  # cd by typing directory name without cd
setopt multios                  # enable multiple redirections
setopt prompt_subst             # enable parameter expansion in prompt

# ===== Directory Navigation =====
setopt auto_pushd               # make cd push old dir to dir stack

# ===== Globbing and Expansion =====
setopt extended_glob            # enable extended globbing
setopt numeric_glob_sort        # sort numbered files numerically
unsetopt case_glob              # case-insensitive globbing

# ===== Keybindings =====
bindkey -e
bindkey '^[[1;5C' forward-word      # Ctrl+Right
bindkey '^[[1;5D' backward-word     # Ctrl+Left
bindkey '^H' vi-backward-kill-word

# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
# <<< grok installer <<<

# opencode
export PATH="$HOME/.opencode/bin:$PATH"
