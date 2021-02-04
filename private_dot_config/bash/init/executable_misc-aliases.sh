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

#Git aliases
alias g='git'
alias gd='git diff'
alias s='git status -s'
alias lg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias git-meld='git difftool  -d -t meld'

#We call date with sudo to make the script ask the pasword right away instead of after printing the date
alias sync-system-time='sudo date && echo "Updating..." && sudo ntpdate -s us.pool.ntp.org && date'
alias cal='ncal -C'

if hash nvim 2>/dev/null; then
  alias vim=nvim
fi
