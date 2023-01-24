# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#......
alias ..='cd ..'
alias ....='cd ../..'
alias ......='cd ../../..'
alias ........='cd ../../../..'
alias ..........='cd ../../../../..'

alias node='NODE_PATH="$(npm root -g):$NODE_PATH" node'

#We call date with sudo to make the script ask the pasword right away instead of after printing the date
alias sync-system-time='sudo date && echo "Updating..." && sudo ntpdate -s us.pool.ntp.org && date'
alias cal='ncal -C'

if hash nvim 2>/dev/null; then
  alias vim=nvim
fi

