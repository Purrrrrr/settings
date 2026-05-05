# How to run

Quick start:

`bash <(wget -qO - https://github.com/Purrrrrr/settings/raw/master/setup.sh)`

This command installs the required SSH keys and clones the repository to ~/.settings
It then proceeds in installing required libraries.

Then you must run

`python3 ./setup_vars.py`

The command will ask to choose what configurations and packages to install.
You can then run:

`ansible-playbook --vault-password-file vault_pass -K playbook.yml`


## TODO

Missing packages and programs I would like to automate:

* eslint/eslint_d
* IntelliJ Idea (what about licenses?)
* AWS cli
* The XMonad session
* Some good coding font

Other stuff

* Organize the bash scripts better, enable some kind of toggling mechanism perhaps?
