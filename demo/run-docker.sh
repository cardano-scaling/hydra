#!/usr/bin/env bash

# Prepare environment to run the demo cluster, then launches docker-compose demo,
# passing arguments to the script to docker-compose executable.
# If there's already a demo running, bail out.
set -e

BASEDIR=$(realpath $(dirname $(realpath $0))/..)
COMPOSE_YAML="$BASEDIR/demo/docker-compose.yaml"

# Sanity check to prevent accidentally tripping oneself with an existing demo
if ( docker-compose -f $COMPOSE_YAML ps | grep hydra-node > /dev/null 2>&1 ); then
  echo >&2 -e "# Demo already in progress, exiting"
  echo >&2 -e "# To stop the demo use: docker-compose down"
  exit 1
fi

function dc() {
  docker-compose -f ${COMPOSE_YAML} ${@}
}

"$BASEDIR/demo/prepare-devnet.sh"
dc up -d cardano-node
"$BASEDIR/demo/seed-devnet.sh"
dc --profile hydra-node up -d
echo >&2 -e "\n# Launch TUI on hydra-node-1: docker-compose --profile tui run hydra-tui-1"
echo >&2 -e "\n# Stop the demo: docker-compose down\n"
dc --profile tui run hydra-tui-1
