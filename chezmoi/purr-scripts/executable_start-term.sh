#!/bin/bash

start-term-with-wait() {
  start-term $1 "echo 'Waiting $2 seconds to run \"$3\", press any key to start right now'; read -t$2 -n1; $3"
}

start-term() {
  if [ -z "$2" ]
  then
    mate-terminal --working-directory $1
  else
    mate-terminal -x bash -c "cd $1; (trap bash SIGINT; ${@:2} && bash || bash)"
  fi
}

if [ "$0" -ef "$BASH_SOURCE" ]; then
  start-term ${@}
fi
