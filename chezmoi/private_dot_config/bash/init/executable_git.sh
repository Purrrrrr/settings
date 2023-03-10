#Git aliases
alias g='git'
alias gd='git diff'
alias gca='git commit --amend'
alias ga='git add'
alias s='git status -s'
alias lg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias git-meld='git difftool  -d -t meld'

function current-ticket() {
  rev=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  regex=$(cat ~/.config/bash/init/git-ticket-regex.txt)
  grep -Po $regex <<< "$rev"
}
export -f current-ticket

function gc() {
  ticket=$(current-ticket)

  if [[ -z $ticket ]]; then 
    git commit "$@";
  else
    git commit -m "$ticket": -e "$@";
  fi;
}

export -f gc
