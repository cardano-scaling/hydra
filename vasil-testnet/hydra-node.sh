#!/usr/bin/env bash

exec cabal exec hydra-node -- \
  --node-id 314 \
  --node-socket "ipc/node.socket" \
  --network-id 9 \
  --ledger-genesis "config/shelley-genesis.json" \
  --ledger-protocol-parameters "config/protocol-parameters.json" \
  --hydra-signing-key "credentials/sebastian.hydra.sk" \
  --cardano-signing-key "credentials/sebastian.cardano.sk" \
  --start-chain-from "440772.7870838f05071a30b364832490cfbf9f882382a7d5524e821debaad099c4ed99"
