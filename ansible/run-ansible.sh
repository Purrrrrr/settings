#!/bin/bash
DIR=$(dirname $(readlink -f "$BASH_SOURCE"))

main() {
  echo $DIR/vars.json
  if [[ ! -f $DIR/vars.json ]] || ask_yes_no "Setup vars?"; then
    python3 $DIR/setup_vars.py
  fi

  if ask_yes_no "Proceed with install?"; then
    ansible-playbook --vault-password-file $DIR/vault_pass -K $DIR/playbook.yml
  fi
}

ask_yes_no() {
  local prompt="${1:-Continue?}"
  local reply

  while true; do
    read -rp "$prompt [y/n]: " reply
    case "$reply" in
      [Yy]|[Yy][Ee][Ss]) return 0 ;;
      [Nn]|[Nn][Oo])     return 1 ;;
      *) echo "Please answer y or n." ;;
    esac
  done
}

main
