#!/usr/bin/env bash

# magic=9 # vasil testnet
magic=1097911063 # testnet

export CARDANO_NODE_SOCKET_PATH=ipc/node.socket
cardano-cli query protocol-parameters \
    --testnet-magic ${magic} > protocol-parameters.json

exec cabal exec hydra-node -- \
  --node-id 314 \
  --node-socket "ipc/node.socket" \
  --network-id ${magic} \
  --ledger-genesis "cardano-configurations/network/testnet/genesis/shelley.json" \
  --ledger-protocol-parameters "protocol-parameters.json" \
  --hydra-signing-key "credentials/sebastian.hydra.sk" \
  --cardano-signing-key "credentials/sebastian.cardano.sk"
