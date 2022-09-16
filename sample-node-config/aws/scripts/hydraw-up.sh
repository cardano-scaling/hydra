#! /bin/bash -xe
# Run hydraw instance

# fail if something goes wrong
set -e

export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/preview/genesis/shelley.json)

# this is manually hardcoded from https://github.com/input-output-hk/hydra-poc/releases/tag/0.7.0
# perhaps there would be a way to look those up in the Chain?
export HYDRA_SCRIPTS_TX_ID=bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246

docker-compose --profile hydraw up -d
