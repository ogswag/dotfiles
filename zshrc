export PS1="%n@%m %F{167}%1~%f %% "

GPG_TTY=$(tty)
export GPG_TTY

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="$HOME/git-personal/scripts:$PATH"

alias ll="gls --group-directories-first -Flh --color=auto"