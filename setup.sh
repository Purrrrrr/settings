#!/bin/bash
KEYFILE=id_rsa.github
SETTINGS_DIR=~/settings

function main {
  # Install first prequisites
  echo "Installing git and ansible"
  sudo apt-get -qqy install ansible git &&

  # Download and unencrypt GitHub SSH key
  echo ""
  echo "Make sure we have SSH keys..."
  downloadKeys &&
  echo ""

  TMP=$(mktemp)

  while keysEncrypted ~/.ssh/$KEYFILE; do
    echo "Decrypting keys..."
    askVaultPass $TMP
    if ansible-vault decrypt --vault-password-file $TMP ~/.ssh/$KEYFILE; then break; fi
  done

  echo ""
  echo "Trying to clone settings repository"
  git clone git@github.com:Purrrrrr/settings.git $SETTINGS_DIR

  echo ""
  echo "Installing ansible dependencies"
  $SETTINGS_DIR/ansible/./prequisites.sh

  if [[ ! -s $TMP ]]; then
    #Make sure we have a vault pass
    askVaultPass $TMP
  fi
  mv $TMP $SETTINGS_DIR/ansible/vault_pass

  echo ""
  echo "-----"
  echo "READY"
  echo "-----"
  echo ""
  echo "Try running:"
  echo "cd $SETTINGS_DIR/ansible"
  echo "ansible-playbook -i inventories/localhost --vault-password-file vault_pass -K playbook.yml"
}

function downloadKeys {
  if [[ ! -f  ~/.ssh/$KEYFILE.pub ]]; then
    wget -nc -O ~/.ssh/$KEYFILE.pub https://github.com/Purrrrrr/settings/raw/master/ssh/$KEYFILE.pub
  fi
  if [[ ! -f  ~/.ssh/$KEYFILE ]]; then
    wget -nc -O ~/.ssh/$KEYFILE https://github.com/Purrrrrr/settings/raw/master/ssh/$KEYFILE
  fi

  if [[ ! -f  ~/.ssh/$KEYFILE.pub ]]; then
    echo "Error downloading public key file"
    exit 1
  fi
  if [[ ! -f  ~/.ssh/$KEYFILE ]]; then
    echo "Error downloading private key file"
    exit 1
  fi

  chmod 0600 ~/.ssh/$KEYFILE ~/.ssh/$KEYFILE.pub
}

function keysEncrypted {
  grep -qL VAULT $1
}

function askVaultPass {
  echo -n "Provide ansible vault password: "
  read -s ANSIBLE_PASS
  echo ""

  echo $ANSIBLE_PASS > $1
}

main
