#Git aliases
alias g='git'
alias gd='git diff'
alias gca='git commit --amend'
alias ga='git add'
alias s='git status -s'
alias lg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias git-meld='git difftool  -d -t meld'

#alias gc='git commit -m $(git rev-parse --abbrev-ref HEAD | egrep -o "[a-zA-Z]+-[0-9]+(/[a-zA-Z]+-[0-9]+)?") -e'
function gc() {
  rev=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  regex="[a-zA-Z]+-[0-9]+(/[a-zA-Z]+-[0-9]+)?"
  ticket=$(egrep -o $regex <<< "$rev")

  if [[ -z $ticket ]]; then 
    git commit "$@";
  else
    git commit -m "$ticket" -e "$@";
  fi;
}

export -f gc
