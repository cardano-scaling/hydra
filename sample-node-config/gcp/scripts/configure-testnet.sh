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

ln -s cardano-configurations/network/preview/ devnet

docker pull ghcr.io/input-output-hk/mithril-client:latest
SNAPSHOT=$(curl -s https://aggregator.api.mithril.network/aggregator/snapshots | jq -r .[0].digest)

mithril_client () {
  docker run --rm -ti -e NETWORK=testnet -v $(pwd):/data -e AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator -w /data -u $(id -u) ghcr.io/input-output-hk/mithril-client:latest $@
}

echo "Restoring snapshot $SNAPSHOT"
mithril_client show $SNAPSHOT

mithril_client download $SNAPSHOT

mithril_client restore $SNAPSHOT


# run docker
docker-compose up -d
