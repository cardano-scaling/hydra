#! /bin/bash -xe
# Configure environment for user ubuntu

# fail if something goes wrong
set -e

export KEY_NAME=$KEY_NAME
echo "export KEY_NAME=$KEY_NAME" >> ~/.bash_env

export NETWORK=$ENV
echo "export NETWORK=$NETWORK" >> ~/.bash_env

echo "Running $KEY_NAME-$NETWORK"

echo "Getting cardano network configuration"
git clone https://github.com/input-output-hk/cardano-configurations repos/cardano-configurations
ln -s repos/cardano-configurations/network/$NETWORK network

echo "Including hydra env variables"
export NETWORK_MAGIC=$(jq .networkMagic repos/cardano-configurations/network/$NETWORK/genesis/shelley.json)
echo "export NETWORK_MAGIC=$NETWORK_MAGIC" >> ~/.bash_env

# this is manually hardcoded from https://github.com/cardano-scaling/hydra/releases/tag/0.10.0
# perhaps there would be a way to look those up in the Chain?
export HYDRA_SCRIPTS_TX_ID=$(jq -r '."'$NETWORK'".hydraScriptsTxId' ~/scripts/configure.json)
echo "export HYDRA_SCRIPTS_TX_ID=$HYDRA_SCRIPTS_TX_ID" >> ~/.bash_env

# Mithril stuff
# resource: https://mithril.network/doc/manual/developer-docs/nodes/mithril-aggregator
echo "Pulling mithril"
export MITHRIL_IMAGE_ID=latest
echo "export MITHRIL_IMAGE_ID=$MITHRIL_IMAGE_ID" >> ~/.bash_env

docker pull ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID

export AGGREGATOR_ENDPOINT=$(jq -r '."'$NETWORK'".mithril.aggregatorEndpoint' ~/scripts/configure.json)
echo "export AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT" >> ~/.bash_env

export GENESIS_VERIFICATION_KEY_URL=$(jq -r '."'$NETWORK'".mithril.genesisVerificationKeyURL' ~/scripts/configure.json)
echo "export GENESIS_VERIFICATION_KEY_URL=$GENESIS_VERIFICATION_KEY_URL" >> ~/.bash_env

export GENESIS_VERIFICATION_KEY=$(wget -q -O - $GENESIS_VERIFICATION_KEY_URL)
echo "export GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY" >> ~/.bash_env

export ANCILLARY_VERIFICATION_KEY_URL=$(jq -r '."'$NETWORK'".mithril.ancillaryVerificationKeyURL' ~/scripts/configure.json)
echo "export GENESIS_VERIFICATION_KEY_URL=$GENESIS_VERIFICATION_KEY_URL" >> ~/.bash_env

export ANCILLARY_VERIFICATION_KEY=$(wget -q -O - $ANCILLARY_VERIFICATION_KEY_URL)
echo "export ANCILLARY_VERIFICATION_KEY=$ANCILLARY_VERIFICATION_KEY" >> ~/.bash_env

export SNAPSHOT_DIGEST=$(curl -s $AGGREGATOR_ENDPOINT/artifact/snapshots | jq -r '.[0].digest')
echo "export SNAPSHOT_DIGEST=$SNAPSHOT_DIGEST" >> ~/.bash_env

mithril_client () {
  docker run --rm --workdir /app/data -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e ANCILLARY_VERIFICATION_KEY=$ANCILLARY_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}

echo "Restoring snapshot $SNAPSHOT_DIGEST"
# Show detailed information about a snapshot
mithril_client cardano-db snapshot show $SNAPSHOT_DIGEST
# Download the given snapshot and verify the certificate
# This downloads and restores a snapshot
mithril_client cardano-db download $SNAPSHOT_DIGEST

# FIXME
mv -f ./db network/

