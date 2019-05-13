function make_color_prompt
{
    local none="\[\033[0m\]"

    local black="\[\033[0;30m\]"
    local dark_gray="\[\033[1;30m\]"
    local blue="\[\033[0;34m\]"
    local light_blue="\[\033[1;34m\]"
    local green="\[\033[0;32m\]"
    local light_green="\[\033[1;32m\]"
    local cyan="\[\033[0;36m\]"
    local light_cyan="\[\033[1;36m\]"
    local red="\[\033[0;31m\]"
    local light_red="\[\033[1;31m\]"
    local purple="\[\033[0;35m\]"
    local light_purple="\[\033[1;35m\]"
    local brown="\[\033[0;33m\]"
    local yellow="\[\033[1;33m\]"
    local light_gray="\[\033[0;37m\]"
    local white="\[\033[1;37m\]"

    local current_tty=`tty | sed -e "s/\/dev\/\(.*\)/\1/"`

    local u_color=$purple
    id -u > /dev/null 2>&1 &&           #Cross-platform hack.
        if [ `id -u` -eq 0 ] ; then
            local u_color=$red
        fi

    PS1="$red\u@\H$dark_gray:$none\w\n\$ "
    PS2="$dark_gray>$none "

}

make_color_prompt

GIT_PROMPT_START=$( echo $PS1 | sed 's/\\n\$//')
GIT_PROMPT_END="\n\$ "
GIT_PROMPT_ONLY_IN_REPO=1
source ~/.config/bash/init/bash-git-prompt/gitprompt.sh

function empty_command_prompt
{
  echo ----
  git status -s 2>/dev/null && echo ----
  ls
}

function execute_when_no_command
{
  HISTCMD_previous=$(fc -l -1); HISTCMD_previous=${HISTCMD_previous%%$'[\t ]'*}
  if [[ -n $HISTCMD_before_last ]]; then
    if [[ $HISTCMD_before_last = "$HISTCMD_previous" ]]; then
      # cancelled prompt
      $1;
    fi
  fi
  HISTCMD_before_last=$HISTCMD_previous;
}
#PROMPT_COMMAND="execute_when_no_command empty_command_prompt;$PROMPT_COMMAND"

function print_vim_depth
{
  VIMDEPTH=$(pgrep -c vim -t $(ps h otty $$))
  if [[ $VIMDEPTH -gt 0 ]]; then
    echo "You have $VIMDEPTH vim processes in the background";
  fi
}
PROMPT_COMMAND="$PROMPT_COMMAND;print_vim_depth"
