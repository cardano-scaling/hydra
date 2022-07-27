#!/usr/bin/env bash

# Prepare environment to run the demo cluster, then launches docker-compose demo,
# passing arguments to the script to docker-compose executable.
# If there's already a demo running, bail out.
set -e

BASEDIR=$(realpath $(dirname $(realpath $0))/..)
COMPOSE_YAML="$BASEDIR/demo/docker-compose.yaml"

# Sanity check to prevent accidentally tripping oneself with an existing demo
if ( docker compose -f $COMPOSE_YAML ps | grep hydra-node | grep Up > /dev/null 2>&1 ); then
  echo "Demo already in progress, exiting"
  exit 1
fi

"$BASEDIR/demo/prepare-devnet.sh"

exec docker compose -f "$COMPOSE_YAML" up "$@"
