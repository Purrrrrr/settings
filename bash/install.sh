#!/bin/bash
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ln -si $CURRENT_DIR/bashrc $HOME/.bashrc
mkdir -p $HOME/.config/bash
mkdir    $HOME/.config/bash/init
mkdir    $HOME/.config/bash/paths
ln -si $CURRENT_DIR/autorun.sh $HOME/.config/bash/autorun.sh
ln -si $HOME/.local/bin $HOME/.config/bash/paths/home-local-bin
