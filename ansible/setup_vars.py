from questionary import checkbox, Choice
from os import path
import yaml
import json

dir = path.dirname(path.realpath(__file__))

roles = dict(
  xmonad = "XMonad and tools for it",
  nvm = "Node version manager",
  jEnv = "jEnv Java version manager",
  copilotCLI = "Install copilot cli using node",
  starship = "Starship shell",
  dotfiles = "Install and setup chezmoi",
  applyDotfiles = "Apply chezmoi dotfiles",
  aisleriotBonded = "Bonded aisleriot cards deck",
  ubuntuNerdFont = "Ubuntu Nerd Font for neovim and other CLI tools",
  docker = "Docker",
)

varsFile = dir + "/vars.json"

if path.exists(varsFile):
    with open(varsFile, "r") as f:
        selected = json.load(f)["chosen"]
else:
    selected = dict(snapPackages = [], aptPackages = [], roles = {})

with open(dir + "/vars/packages.yml", "r") as f:
    choices = yaml.safe_load(f)

chosenSnapPackages = checkbox(
  "What snap packages to install?",
  choices=[Choice(choice["name"], value = choice["pkg"], checked = choice["pkg"] in selected["snapPackages"]) for choice in choices["snapPackages"]]
).ask()

chosenAptPackages = checkbox(
  "What apt packages to install?",
  choices=[Choice(choice["name"], value = choice["pkg"], checked = choice["pkg"] in selected["aptPackages"]) for choice in choices["aptPackages"]]
).ask()

chosenRoles = checkbox(
  "What roles to install?",
  choices=[Choice(roles[key], value=key, checked = selected["roles"].get(key)) for key in roles]
).ask()

with open(dir+"/vars.json", "w") as f:
  json.dump(dict(chosen = dict(
      snapPackages = chosenSnapPackages or [],
      aptPackages = chosenAptPackages or [],
      roles = {role: role in chosenRoles for role in roles},
  )), f)
