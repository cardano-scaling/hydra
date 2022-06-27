#!/usr/bin/env bash

exec cardano-node run \
  --config cardano-configurations/network/testnet/cardano-node/config.json \
  --topology cardano-configurations/network/testnet/cardano-node/topology.json \
  --database-path db \
  --socket-path ipc/node.socket
