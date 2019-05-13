#!/bin/bash
XMONAD_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo "Installing haskell stack"
curl -sSL https://get.haskellstack.org | sudo sh

echo "Installing dependencies to compile XMonad"
sudo apt install libx11-dev libxinerama-dev libxext-dev libxrandr-dev libxss-dev libxft-dev libghc-xmonad-contrib-dev libghc-xmonad-dev suckless-tools

echo "Installing XMonad"
stack install xmonad

echo "Installing XMonad configuration to ~/.xmonad"
ln -s XMONAD_DIR ~/.xmonad

echo "Ready, now go do some session magic to enable it"
