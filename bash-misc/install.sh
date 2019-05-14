#!/bin/bash
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ln -si $CURRENT_DIR/misc.sh $HOME/.config/bash/init/misc.sh
ln -si $CURRENT_DIR/misc-aliases.sh $HOME/.config/bash/init/misc-aliases.sh
