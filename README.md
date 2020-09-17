# How to run

To install the settings go to the ansible directory and run:
`prequisites.sh`

and then:
`ansible-playbook -i inventories/localhost --ask-vault-pass -K playbook.yml`

The command will ask to choose what configurations and packages to install.

You can also preselect some options to install without prompts
`ansible-playbook -i inventories/localhost --extra-vars '{"userChoices": ["xmonad"]}' --ask-vault-pass -K playbook.yml`

## TODO

Missing packages and programs I would like to automate:

* eslint/eslint_d
* Java
* IntelliJ Idea (what about licenses?)
* KeePassXC browser integration
* AWS cli
* The XMonad session

Other stuff

* Organize the bash scripts better, enable some kind of toggling mechanism perhaps?
