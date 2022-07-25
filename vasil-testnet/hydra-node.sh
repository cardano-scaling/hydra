#!/usr/bin/env bash

exec cabal exec hydra-node -- \
  --node-id 314 \
  --node-socket "ipc/node.socket" \
  --network-id 9 \
  --ledger-genesis "config/shelley-genesis.json" \
  --ledger-protocol-parameters "config/protocol-parameters.json" \
  --hydra-signing-key "credentials/alice.hydra.sk" \
  --cardano-signing-key "credentials/alice.cardano.sk" \
  --start-chain-from "1076650.27eb7e85379a0021f3bd80081b3af75beacc256deb016c777d6b1b4f2ef92d62"
