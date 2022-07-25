cabal exec cardano-node -- run \
  --config ${DEVNET}/cardano-node.json \
  --topology ${DEVNET}/topology.json \
  --database-path ${DEVNET}/db \
  --socket-path ${DEVNET}/ipc/node.socket \
  --shelley-kes-key ${DEVNET}/credentials/stake-pool-1/kes.skey \
  --shelley-vrf-key  ${DEVNET}/credentials/stake-pool-1/vrf.skey \
  --shelley-operational-certificate ${DEVNET}/credentials/stake-pool-1/opcert.cert \
  --byron-delegation-certificate  ${DEVNET}/credentials/stake-pool-1/byron-delegation.cert \
  --byron-signing-key  ${DEVNET}/credentials/stake-pool-1/byron-delegate.key \

