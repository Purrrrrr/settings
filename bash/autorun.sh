if [[ -z "${BASH_CONFIG}" ]]; then
  echo "$0: $$BASH_CONFIG is not set!"
  return
fi

export $BASH_INIT_SCRIPTS=$BASH_CONFIG/init

# iterate over our bashrc script files
for script in $BASH_INIT_SCRIPTS/*.sh
do
    # check if the script is executable
    if [ -x "${script}" ]; then
        # run the script
        source ${script}
    fi
done

#Add stuff to the PATH variable
for path in $BASH_CONFIG/paths/*
do
    # check if the path is a link
    if [ -L "${path}" ]; then
        # run the script
        PATH="$PATH:$(readlink $path)"
    fi
done
