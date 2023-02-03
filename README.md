# How to run

Quick start:

`wget -qO - https://github.com/Purrrrrr/settings/raw/master/setup.sh | bash`

This command installs the required SSH keys and clones the repository to ~/.settings
It then proceeds in installing required libraries.

You can then run:
`ansible-playbook -i inventories/localhost --vault-password-file vault_pass -K playbook.yml`

The command will ask to choose what configurations and packages to install.

You can also preselect some options to install without prompts
`ansible-playbook -i inventories/localhost --extra-vars '{"userChoices": ["xmonad"]}' --vault-password-file vault_pass -K playbook.yml`

## TODO

Missing packages and programs I would like to automate:

* eslint/eslint_d
* IntelliJ Idea (what about licenses?)
* AWS cli
* The XMonad session
* Try out https://github.com/SBoudrias/Inquirer.js/

Other stuff

* Organize the bash scripts better, enable some kind of toggling mechanism perhaps?
