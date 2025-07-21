
#!/usr/bin/env bash

set -euo pipefail

cleanup() {
  echo "--- Cleaning up"
  pkill -P $$ || true
}

trap cleanup EXIT

echo "--- Setup devnet"
./prepare-devnet.sh

echo "--- Starting cardano-node"
cardano-node run \
  --config devnet/cardano-node.json \
  --topology devnet/topology.json \
  --database-path devnet/db \
  --socket-path devnet/node.socket \
  --shelley-operational-certificate devnet/opcert.cert \
  --shelley-kes-key devnet/kes.skey \
  --shelley-vrf-key devnet/vrf.skey > devnet/cardano-node.log 2>&1 &

echo "--- Waiting for socket"
while [ ! -S "devnet/node.socket" ]; do
  sleep 1
done
echo "Socket created."

echo "--- Seeding devnet"
export CARDANO_NODE_SOCKET_PATH=devnet/node.socket
./seed-devnet.sh $(which cardano-cli) $(which hydra-node) --mainnet-era babbage

echo "--- Starting hydra-node"
source .env
hydra-node \
  --node-id 1 \
  --api-port 4001 \
  --monitoring-port 6001 \
  --peer 127.0.0.1:5002 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
  --persistence-dir devnet/persistence/alice > devnet/hydra-node.log 2>&1 &

sleep 2
echo "--- Checking for hydra-node"
if cat devnet/hydra-node.log | grep '"tag":"APIServerStarted"'; then
  echo "OK"
else
  echo "FAIL"
  cat devnet/hydra-node.log
  exit 1
fi

