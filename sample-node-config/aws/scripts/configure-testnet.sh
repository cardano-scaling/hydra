#! /bin/bash -xe

# fail if something goes wrong
set -e

echo 'Configure environment for user ubuntu'

echo 'update system'
sudo apt update -y

echo 'installing jq'
sudo apt-get install jq -y

# https://docs.docker.com/engine/install/ubuntu/
echo 'installing docker'
sudo curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh ./get-docker.sh
# accept github.com key
sudo ssh-keyscan github.com >> ~/.ssh/known_hosts

echo 'adding docker to ubutunu user'
sudo usermod -a -G docker ubuntu

echo 'installing docker-compose'
sudo curl -L https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

cd /home/ubuntu
touch .bashrc
echo "alias reload='source .bashrc'"
echo "alias logs='cat /var/log/cloud-init-output.log'" >> .bashrc
echo "alias g=git" >> .bashrc
echo "alias d=docker" >> .bashrc
echo "alias dc=docker-compose" >> .bashrc

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations
export NETWORK_MAGIC=$(jq .networkMagic cardano-configurations/network/testnet/genesis/shelley.json)

# this is manually hardcoded from https://github.com/input-output-hk/hydra-poc/releases/tag/0.7.0
# perhaps there would be a way to look those up in the Chain?
export HYDRA_SCRIPTS_TX_ID=bde2ca1f404200e78202ec37979174df9941e96fd35c05b3680d79465853a246

ln -s cardano-configurations/network/preview devnet

# Mithril stuff
sudo docker pull ghcr.io/input-output-hk/mithril-client:latest
SNAPSHOT=$(curl -s https://aggregator.api.mithril.network/aggregator/snapshots | jq -r .[0].digest)

mithril_client () {
  sudo docker run --rm -ti -e NETWORK=testnet -v $(pwd):/data -e AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator -w /data -u $(id -u) ghcr.io/input-output-hk/mithril-client:latest $@
}

echo "Restoring snapshot $SNAPSHOT"
mithril_client show $SNAPSHOT
mithril_client download $SNAPSHOT
mithril_client restore $SNAPSHOT

mv -f data/testnet/${SNAPSHOT}/db devnet/

sudo docker-compose --profile hydraw up -d



