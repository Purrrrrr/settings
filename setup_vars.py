from questionary import checkbox, Choice
from os import path
import json

snapPackages = [
 { "pkg": "chromium", "classic": True },
 { "pkg": "intellij-idea", "classic": True },
 { "pkg": "nvim", "classic": True },
 { "pkg": "telegramp-desktop", "classic": False },
 { "pkg": "code", "classic": True },
]

chosenSnapPackages = checkbox(
  "What snap packages to install?",
  choices=[choice["pkg"] for choice in snapPackages]
).ask()

aptPackages = [
  "openssh-server",
  "gimp",
  "nginx",
  "iotop",
  "docker.io",
  "pandoc",
]

chosenAptPackages = checkbox(
  "What apt packages to install?",
  choices=aptPackages
).ask()

roles = dict(
  xmonad = "XMonad and tools for it",
  nvm = "Node version manager",
  copilotCLI = "Install copilot cli using node",
  starship = "Starship shell",
  dotfiles = "Install and setup chezmoi",
  applyDotfiles = "Apply chezmoi dotfiles",
  aisleriotBonded = "Bonded aisleriot cards deck",
  ubuntuNerdFont = "Ubuntu Nerd Font for neovim and other CLI tools",
)

chosenRoles = checkbox(
  "What roles to install?",
  choices=[Choice(roles[key], value=key) for key in roles]
).ask()

dir = path.dirname(path.realpath(__file__))
with open(dir+"/ansible/vars.json", "w") as f:
  json.dump(dict(
      userSnapPackages = [pkg for pkg in snapPackages if pkg["pkg"] in chosenSnapPackages],
      userAptPackages = chosenAptPackages,
      userRoles = {role: role in chosenRoles for role in roles},
  ), f)
