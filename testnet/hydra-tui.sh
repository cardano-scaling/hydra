#!/usr/bin/env bash

# magic=9 # vasil testnet
magic=1097911063 # testnet

exec cabal exec hydra-tui -- \
  --connect "0.0.0.0:4001" \
  --cardano-signing-key "credentials/sebastian.cardano.sk" \
  --network-id ${magic} \
  --node-socket "ipc/node.socket"
