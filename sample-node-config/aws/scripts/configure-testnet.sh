#! /bin/bash -xe
# Configure environment for user ubuntu

# fail if something goes wrong
set -e

echo "Getting cardano network configuration"
git clone https://github.com/input-output-hk/cardano-configurations
ln -s cardano-configurations/network/preprod devnet

echo "Including hydra env variables"
export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/preprod/genesis/shelley.json)
echo "export NETWORK_MAGIC=$NETWORK_MAGIC" >> /home/ubuntu/.bashrc

# this is manually hardcoded from https://github.com/input-output-hk/hydra/releases/tag/0.9.0
# perhaps there would be a way to look those up in the Chain?
export HYDRA_SCRIPTS_TX_ID=6fd13073c47411af7f3adf31f46e61f570872a832822fdc5da5b214766651bfd
echo "export HYDRA_SCRIPTS_TX_ID=$HYDRA_SCRIPTS_TX_ID" >> /home/ubuntu/.bashrc

# Mithril stuff
echo "Pulling mithril"
export MITHRIL_IMAGE_ID=unstable

docker pull ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID

export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator

export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)

export SNAPSHOT_DIGEST=$(curl -s $AGGREGATOR_ENDPOINT/snapshots | jq -r '.[0].digest')

export NETWORK=preprod

mithril_client () {
  docker run --rm -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}

echo "Restoring snapshot $SNAPSHOT_DIGEST"
mithril_client show $SNAPSHOT_DIGEST
mithril_client download $SNAPSHOT_DIGEST
mithril_client restore $SNAPSHOT_DIGEST

mv -f ./$NETWORK/${SNAPSHOT_DIGEST}/db devnet/

