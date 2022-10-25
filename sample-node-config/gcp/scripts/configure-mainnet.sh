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

ln -s cardano-configurations/network/mainnet mainnet

docker-compose up -d
