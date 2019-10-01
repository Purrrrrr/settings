#!/bin/bash

is-installed() {
	apt -qq list --installed $1 | egrep -q '\[installed\]$'
}

if ! is-installed dialog; then
	echo "Installing dialog..."
	sudo apt install dialog
fi

cmd=(dialog --separate-output --checklist "Select packages to install:" 22 76 16)
options=(git "Git" on
         neovim "Neovim" on
         'openssh-client openssh-server' "SSH client and server" on
         'openconnect   network-manager-openconnect*' "Solita VPN prequisites" on)
choices=$("${cmd[@]}" "${options[@]}" 2>&1 >/dev/tty)
clear

echo "Installing packages $choices"
sudo apt install $choices
