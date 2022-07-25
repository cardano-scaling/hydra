cabal exec hydra-tui -- \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key devnet/credentials/alice.sk \
  --network-id 42 \
  --node-socket devnet/ipc/node.socket
