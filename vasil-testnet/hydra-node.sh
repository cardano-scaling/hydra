#!/usr/bin/env bash

# Query network's protocol parameters and re-use them for hydra-node
export CARDANO_NODE_SOCKET_PATH=ipc/node.socket
cardano-cli query protocol-parameters --testnet-magic 9 > protocol-parameters.json

exec cabal exec hydra-node -- \
  --node-id 314 \
  --node-socket "ipc/node.socket" \
  --network-id 9 \
  --ledger-genesis "config/shelley-genesis.json" \
  --ledger-protocol-parameters "protocol-parameters.json" \
  --hydra-signing-key "credentials/sebastian.hydra.sk" \
  --cardano-signing-key "credentials/sebastian.cardano.sk" \
  --start-chain-from "440772.7870838f05071a30b364832490cfbf9f882382a7d5524e821debaad099c4ed99"
