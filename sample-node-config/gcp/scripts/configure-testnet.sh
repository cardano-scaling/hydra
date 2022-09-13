#!/bin/bash
# Configure environment for user curry

# fail if something goes wrong
set -e

# accept github.com key
ssh-keyscan github.com >> ~/.ssh/known_hosts

# download gpg key signing testnet dump
curl https://api.github.com/users/abailly-iohk/gpg_keys | jq -r '.[] | .raw_key' | gpg --import

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations

export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/preview/genesis/shelley.json)

# this is manually hardcoded from https://github.com/input-output-hk/hydra-poc/releases/tag/0.7.0
# perhaps there would be a way to look those up in the Chain?
export HYDRA_SCRIPTS_TX_ID=bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246

ln -s cardano-configurations/network/preview devnet

# Mithril stuff
docker pull ghcr.io/input-output-hk/mithril-client:latest
SNAPSHOT=$(curl -s https://aggregator.api.mithril.network/aggregator/snapshots | jq -r .[0].digest)

mithril_client () {
  docker run --rm -ti -e NETWORK=testnet -v $(pwd):/data -e AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator -w /data -u $(id -u) ghcr.io/input-output-hk/mithril-client:latest $@
}

echo "Restoring snapshot $SNAPSHOT"
mithril_client show $SNAPSHOT
mithril_client download $SNAPSHOT
mithril_client restore $SNAPSHOT

mv -f data/testnet/${SNAPSHOT}/db devnet/

docker-compose --profile hydraw up -d

# create marker utxo
chmod +x ./fuel-testnet.sh
exec ./fuel-testnet.sh devnet arnaud.sk 10000000
