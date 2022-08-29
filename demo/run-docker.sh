#!/usr/bin/env bash
#
# Prepare environment to run the demo cluster, then launches docker-compose demo.
# If there's already a demo running, bail out.
set -e

SCRIPT_DIR=$(dirname $(realpath $0))

cd ${SCRIPT_DIR}

# Sanity check to prevent accidentally tripping oneself with an existing demo
if ( docker-compose ps | grep hydra-node > /dev/null 2>&1 ); then
  echo >&2 -e "# Demo already in progress, exiting"
  echo >&2 -e "# To stop the demo use: docker-compose down"
  exit 1
fi

"${SCRIPT_DIR}/prepare-devnet.sh"
docker-compose up -d cardano-node
"${SCRIPT_DIR}/seed-devnet.sh"
docker-compose --profile hydra-node up -d
echo >&2 -e "\n# Launch TUI on hydra-node-1: docker-compose --profile tui run hydra-tui-1"
echo >&2 -e "\n# Stop the demo: docker-compose down\n"
docker-compose --profile tui run hydra-tui-1
