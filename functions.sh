command-found() {
  command -v $1 2>&1
  return $?
}

is-installed() {
	apt -qq list --installed $1 | egrep -q '\[installed\]$'
}
