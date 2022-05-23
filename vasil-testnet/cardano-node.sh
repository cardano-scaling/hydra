#!/usr/bin/env bash

exec cardano-node run \
  --config config/config.json \
  --topology config/topology.json \
  --database-path db \
  --socket-path ipc/node.socket
