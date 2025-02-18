# export PS1="%n@%m %F{red}%1~%f %% "

autoload -Uz add-zsh-hook
# Enable prompt substitution
setopt PROMPT_SUBST
# Initialize command counter
export COMMAND_COUNTER=1

# Increment command counter before each command
function increment_command_counter() {
    COMMAND_COUNTER=$((COMMAND_COUNTER + 1))
}
add-zsh-hook preexec increment_command_counter

# Record start time of each command
function preexec_start() {
    if [[ $(uname) == "Darwin" ]]; then
        # macOS: Use Python to get milliseconds
        export START_TIME=$(python3 -c 'import time; print(int(time.time() * 1000))')
    else
        # Linux (GNU date): Use date +%s%3N
        export START_TIME=$(date +%s%3N)
    fi
}
add-zsh-hook preexec preexec_start

# Generate top border of the box
function generate_top_border() {
    # Colors
    local BLUE='\033[0;34m'
    local CYAN='\033[0;36m'
    local RED='\033[0;31m'
    local NC='\033[0m' # No Color
    # Get terminal width
    local TERMINAL_WIDTH=$(tput cols)
    local LINE_WIDTH=$((TERMINAL_WIDTH - 10))

    # Ensure the line width is non-negative
    if ((LINE_WIDTH < 0)); then
        local LINE_WIDTH=0
    fi

    # Create the top border line
    local BOTTOM_LINE="╰${PADDING}${BLUE}${MESSAGE}${BLUE}─╯"

    local LEFT_SIDE="[Command #${COMMAND_COUNTER}]─[%~]"
    local RIGHT_SIDE="[Time: %*]"

    local COUNTER="[Command #${COMMAND_COUNTER}]"
    local COUNTER_LENGTH=${#COUNTER}
    local CURRENT_DIR=$(pwd)
    local CURRENT_DIR_WITH_HOME=$(echo "$CURRENT_DIR" | sed "s|$HOME|~|")
    # local DIR_NAME=$(basename "$CURRENT_DIR_WITH_HOME")
    local DIR_NAME_LENGTH=${#CURRENT_DIR_WITH_HOME}
    local DIR_NAME_LENGTH=$((DIR_NAME_LENGTH + 3))

    local PADDING_WIDTH=$((LINE_WIDTH - ${DIR_NAME_LENGTH} - ${COUNTER_LENGTH} - ${#RIGHT_SIDE}))
    local PADDING=$(printf "%*s" "$PADDING_WIDTH" '' | tr ' ' '─')

    local LEFT_SIDE="${CYAN}[Command #${COMMAND_COUNTER}]${BLUE}─${BLUE}[%~]${BLUE}"
    local RIGHT_SIDE="[Time: %*]"
    local TOP_LINE="╭─${LEFT_SIDE}${PADDING}${RIGHT_SIDE}─╮"

    echo -n "${BLUE}${TOP_LINE}"
}

# Set the prompt
PROMPT='$(generate_top_border)
%F{blue}│%f %n@%m %# '


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
alias vim="nvim"
alias n="nvim"

export PATH="/usr/local/opt/jpeg/bin:$PATH"
# export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

export PATH="/usr/local/sbin:$PATH"
