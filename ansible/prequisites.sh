#!/bin/bash
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

sudo apt-get install ansible dialog
ansible-galaxy install -r $CURRENT_DIR/requirements.yml
