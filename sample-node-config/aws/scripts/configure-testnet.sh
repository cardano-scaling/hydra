#! /bin/bash -xe
# Configure environment for user ubuntu

# fail if something goes wrong
set -e

echo "Getting cardano network configuration"
git clone https://github.com/input-output-hk/cardano-configurations
# [mainnet]
# ln -s cardano-configurations/network/mainnet network
ln -s cardano-configurations/network/preview network

echo "Including hydra env variables"
# [mainnet]
# export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/mainnet/genesis/shelley.json)
export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/preview/genesis/shelley.json)
echo "export NETWORK_MAGIC=$NETWORK_MAGIC" >> /home/ubuntu/.bashrc

# this is manually hardcoded from https://github.com/input-output-hk/hydra/releases/tag/0.10.0
# perhaps there would be a way to look those up in the Chain?
# [mainnet]
# export HYDRA_SCRIPTS_TX_ID=af1a00e23a9b5c3a811d5c265dd25edfc81fd43f0fbf94229c4c0a5ab18aa5de
export HYDRA_SCRIPTS_TX_ID=d237926e174a2ca386174a5810d30f0ca6db352219dd7eacdc7d5969ae75d58f
echo "export HYDRA_SCRIPTS_TX_ID=$HYDRA_SCRIPTS_TX_ID" >> /home/ubuntu/.bashrc

# Mithril stuff (not working for mainnet)
# resource: https://mithril.network/doc/manual/developer-docs/
echo "Pulling mithril"
export MITHRIL_IMAGE_ID=latest

docker pull ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID

export AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator

export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)

export SNAPSHOT_DIGEST=$(curl -s $AGGREGATOR_ENDPOINT/snapshots | jq -r '.[0].digest')

export NETWORK=preview

mithril_client () {
  docker run --rm -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}

echo "Restoring snapshot $SNAPSHOT_DIGEST"
mithril_client show $SNAPSHOT_DIGEST
mithril_client download $SNAPSHOT_DIGEST
mithril_client restore $SNAPSHOT_DIGEST

mv -f ./$NETWORK/${SNAPSHOT_DIGEST}/db network/

