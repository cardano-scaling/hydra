#! /bin/bash -xe
# Configure environment for user ubuntu

# fail if something goes wrong
set -e

echo "Getting cardano network configuration"
git clone https://github.com/input-output-hk/cardano-configurations
ln -s cardano-configurations/network/preview devnet

echo "Including hydra env variables"
NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/preview/genesis/shelley.json)
echo "export NETWORK_MAGIC=$NETWORK_MAGIC" >> /home/ubuntu/.bashrc

# this is manually hardcoded from https://github.com/input-output-hk/hydra-poc/releases/tag/0.7.0
# perhaps there would be a way to look those up in the Chain?
HYDRA_SCRIPTS_TX_ID=bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246
echo "export HYDRA_SCRIPTS_TX_ID=$HYDRA_SCRIPTS_TX_ID" >> /home/ubuntu/.bashrc

# Mithril stuff
echo "Pulling mithril"
docker pull ghcr.io/input-output-hk/mithril-client:latest

GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey) 

SNAPSHOT=$(curl -s https://aggregator.api.mithril.network/aggregator/snapshots | jq -r .[0].digest)

mithril_client () {
  docker run --rm -ti -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e NETWORK=testnet -v $(pwd):/data -e AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator -w /data -u $(id -u) ghcr.io/input-output-hk/mithril-client:latest $@
}

echo "Restoring snapshot $SNAPSHOT"
mithril_client show $SNAPSHOT
mithril_client download $SNAPSHOT
mithril_client restore $SNAPSHOT

mv -f data/testnet/${SNAPSHOT}/db devnet/
