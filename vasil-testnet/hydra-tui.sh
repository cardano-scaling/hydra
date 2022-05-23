#!/usr/bin/env bash

exec cabal exec hydra-tui -- \
  --connect "0.0.0.0:4001" \
  --cardano-signing-key "credentials/sebastian.cardano.sk" \
  --network-id "9" \
  --node-socket "devnet/ipc/node.socket"
