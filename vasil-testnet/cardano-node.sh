#!/usr/bin/env bash

exec cardano-node run \
  --config cardano-node/config.json \
  --topology cardano-node/topology.json \
  --database-path db \
  --socket-path node.socket
