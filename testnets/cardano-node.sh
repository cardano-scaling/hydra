#!/usr/bin/env bash
#
# Run a cardano-node on one of the known public testnets.
set -e

[ ${#} -eq 1 ] && [ -d ${1} ] || {
    echo "Expecting path testnet directory as DIR argument, e.g. 'preview'"
    echo "Usage: $0 DIR"
    exit 1
}
TESTNET_DIR=${1}

cd ${TESTNET_DIR}
exec cardano-node run \
  --config cardano-node/config.json \
  --topology cardano-node/topology.json \
  --database-path db \
  --socket-path node.socket
