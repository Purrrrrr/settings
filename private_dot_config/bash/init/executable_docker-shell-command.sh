function dockershell() {
  ids=$(docker ps -f name="$1" -q)
  idCount=$(wc -w <<< $ids)

  if [[ $idCount -eq 0 ]]; then
    echo "No docker containers match"
  elif [[ $idCount -gt 1 ]]; then
    echo "Too many containers match. Give better input"
    docker ps -f name="$1"
  else
    sh=$2
    if [[ -z $sh ]]; then
      sh=/bin/sh
    fi
    docker exec -it $ids $sh
  fi
}
