#!/bin/bash
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ln -si $CURRENT_DIR/prompt.sh $HOME/.config/bash/init
git clone https://github.com/magicmonty/bash-git-prompt.git $HOME/.config/bash/init-scripts
