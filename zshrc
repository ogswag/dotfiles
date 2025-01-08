# export PS1="%n@%m %F{red}%1~%f %% "

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
    export START_TIME=$SECONDS
}
add-zsh-hook preexec preexec_start

# Calculate and display command duration
function precmd_duration() {
    if [[ -n $START_TIME ]]; then
        local DURATION=$((SECONDS - START_TIME))
        export RPROMPT="%F{cyan}Ran for: $DURATION sec.%f"
        unset START_TIME
    else
        export RPROMPT=""
    fi
    # Print the bottom border on a new line

    LINE=$(printf '%*s' "$TERMINAL_WIDTH" '' | tr ' ' '-')
    BLUE='\033[0;34m'
    if [ "$COMMAND_COUNTER" -ge 2 ]; then
        echo -e "\n%F{blue}╰$(printf '─%.0s' {1..$COLUMNS})╯%f"
    fi
    # echo $'${(r:$COLUMNS::\u2500:)}'
}
add-zsh-hook precmd precmd_duration

# Generate top border of the box
function generate_top_border() {
    local terminal_width=$COLUMNS
    local content="╭─[Time: %*]─[%F{cyan}Command #${COMMAND_COUNTER}%F{blue}]─[%F{yellow}%n@%m%F{blue}]─[%F{red}%~%F{blue}]─"
    # local padding=$(printf '─%.0s' $(seq 1 $((terminal_width - ${#content} - ${#content} ))))
    echo -n "%F{blue}$content╮%f"
}

# Set the prompt
PROMPT='$(generate_top_border)
%F{blue}│%f %# '

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
