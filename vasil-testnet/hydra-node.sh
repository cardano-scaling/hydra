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
  --cardano-signing-key "credentials/sebastian.cardano.sk"
