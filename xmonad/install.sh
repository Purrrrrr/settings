#!/bin/bash
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
XMONAD_DIR="$CURRENT_DIR/settings"

. $CURRENT_DIR/../functions.sh

if ! command-found dzen2; then
	sudo apt install dzen2
fi
if ! command-found xcompmgr; then
	sudo apt install xcompmgr xdotool suckless-tools dzen2 conky wmctrl libxft2:i386 at
fi
if ! command-found xmonad; then
	echo "Installing haskell stack"
	curl -sSL https://get.haskellstack.org | sudo sh

	echo "Installing dependencies to compile XMonad"
	sudo apt install libx11-dev libxinerama-dev libxext-dev libxrandr-dev libxss-dev libxft-dev libghc-xmonad-contrib-dev libghc-xmonad-dev suckless-tools

	echo "Installing XMonad"
	stack install xmonad
fi

echo "Installing XMonad configuration to ~/.xmonad"
ln -s $XMONAD_DIR ~/.xmonad

echo "Ready, now let's do some session magic to enable it"
sudo apt-add-repository ppa:gekkio/xmonad
sudo apt install gnome-session-xmonad
