#!/bin/bash

# Download configuration files from a nominated URL for a nominated network
# If p2p is enabled, create an explicit p2p version, keeping the default aligned with the production
# networks, at least until https://github.com/input-output-hk/ouroboros-network/pull/3844 has been
# inclded in a cardano-node release.

CARDANO_CONFIG_URL=$1
CARDANO_NETWORK=$2

mkdir -p \
  network/$CARDANO_NETWORK/cardano-node \
  network/$CARDANO_NETWORK/genesis \
  network/$CARDANO_NETWORK/cardano-db-sync

SOURCE_TOPOLOGY=$(wget -qO- $CARDANO_CONFIG_URL/$CARDANO_NETWORK/topology.json)
NODE_CONFIG=$(wget -qO- $CARDANO_CONFIG_URL/$CARDANO_NETWORK/config.json | jq '.ByronGenesisFile = "../genesis/byron.json" | .ShelleyGenesisFile = "../genesis/shelley.json" | .AlonzoGenesisFile = "../genesis/alonzo.json" | .ConwayGenesisFile = "../genesis/conway.json"')
DB_SYNC_CONFIG=$(wget -qO- $CARDANO_CONFIG_URL/$CARDANO_NETWORK/db-sync-config.json | jq '.NodeConfigFile = "../cardano-node/config.json"')

wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/byron-genesis.json -O network/$CARDANO_NETWORK/genesis/byron.json
wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/shelley-genesis.json -O network/$CARDANO_NETWORK/genesis/shelley.json
wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/alonzo-genesis.json -O network/$CARDANO_NETWORK/genesis/alonzo.json
wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/conway-genesis.json -O network/$CARDANO_NETWORK/genesis/conway.json

if [ $(echo $SOURCE_TOPOLOGY | jq 'has("publicRoots")') = true ];
then
  ACCESS_POINT=$(echo $SOURCE_TOPOLOGY | jq '.publicRoots[0].accessPoints[0]')

  # Add separate p2p config
  mkdir -p \
      network/${CARDANO_NETWORK}_p2p/cardano-node \
      network/${CARDANO_NETWORK}_p2p/genesis \
      network/${CARDANO_NETWORK}_p2p/cardano-db-sync

  wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/byron-genesis.json -O network/${CARDANO_NETWORK}_p2p/genesis/byron.json
  wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/shelley-genesis.json -O network/${CARDANO_NETWORK}_p2p/genesis/shelley.json
  wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/alonzo-genesis.json -O network/${CARDANO_NETWORK}_p2p/genesis/alonzo.json
  wget -q $CARDANO_CONFIG_URL/$CARDANO_NETWORK/conway-genesis.json -O network/${CARDANO_NETWORK}_p2p/genesis/conway.json

  echo $SOURCE_TOPOLOGY | jq '.' > network/${CARDANO_NETWORK}_p2p/cardano-node/topology.json
  echo $NODE_CONFIG | jq '.' > network/${CARDANO_NETWORK}_p2p/cardano-node/config.json
  echo $DB_SYNC_CONFIG | jq '.' > network/${CARDANO_NETWORK}_p2p/cardano-db-sync/config.json

  # Transform defaults to disable p2p
  jq -nj --argjson address $(echo $ACCESS_POINT | jq '.address') --argjson port $(echo $ACCESS_POINT | jq '.port') '{"Producers": [{"addr": $address, "port": $port, "valency": 1 }]}' > network/$CARDANO_NETWORK/cardano-node/topology.json
  # See https://github.com/input-output-hk/cardano-node/blob/0681cdeb07d81b3b088a6c14e703d03751c3d25d/cardano-node/src/Cardano/Node/Tracing/Tracers/Startup.hs#L366
  echo $NODE_CONFIG | jq '.EnableP2P = false | del(.TestEnableDevelopmentNetworkProtocols)'> network/$CARDANO_NETWORK/cardano-node/config.json
  echo $DB_SYNC_CONFIG | jq '.' > network/$CARDANO_NETWORK/cardano-db-sync/config.json
else
  # Source config doesn't have p2p enabled, so no further transformation required
  echo $SOURCE_TOPOLOGY | jq '.' > network/$CARDANO_NETWORK/cardano-node/topology.json
  echo $NODE_CONFIG | jq '.' > network/$CARDANO_NETWORK/cardano-node/config.json
  echo $DB_SYNC_CONFIG | jq '.' > network/$CARDANO_NETWORK/cardano-db-sync/config.json
fi
