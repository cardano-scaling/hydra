#! /bin/bash -xe
# Configure environment for user ubuntu

# fail if something goes wrong
set -e

[ $# -eq 1 ] || { echo "requires an argument 'github user'"; exit 1 ; }

GH_USER=$1

# accept github.com key
sudo ssh-keyscan github.com >> ~/.ssh/known_hosts

# download gpg key signing testnet dump
curl https://api.github.com/users/$GH_USER/gpg_keys | jq -r '.[] | .raw_key' | gpg --import

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations
ln -s cardano-configurations/network/preview devnet

# Mithril stuff
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
