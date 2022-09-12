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
curl -fsSL https://get.docker.com -o get-docker.sh
sh ./get-docker.sh
# accept github.com key
sudo ssh-keyscan github.com >> ~/.ssh/known_hosts

echo 'installing docker-compose'
sudo curl -L https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose

echo 'adding docker to ubutunu user'
sudo usermod -aG docker ubuntu                  # to add myself to docker group
sudo chgrp docker /usr/local/bin/docker-compose # to give docker-compose to docker group,
sudo chmod +x /usr/local/bin/docker-compose     # to allow docker group users to execute it

cd /home/ubuntu
touch .bashrc
echo "alias reload='source .bashrc'"
echo "alias logs='cat /var/log/cloud-init-output.log'" >> .bashrc
echo "alias g=git" >> .bashrc
echo "alias d=docker" >> .bashrc
echo "alias dc=docker-compose" >> .bashrc

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations
ln -s cardano-configurations/network/preview devnet

# Mithril stuff
# sudo docker pull ghcr.io/input-output-hk/mithril-client:latest
# SNAPSHOT=$(curl -s https://aggregator.api.mithril.network/aggregator/snapshots | jq -r .[0].digest)

# error: missing `genesis_verification_key`
mithril_client () {
  docker run --rm -ti -e NETWORK=testnet -v $(pwd):/data -e AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator -w /data -u $(id -u) ghcr.io/input-output-hk/mithril-client:latest $@
}

echo "Restoring snapshot $SNAPSHOT"
# mithril_client show $SNAPSHOT
# mithril_client download $SNAPSHOT
# mithril_client restore $SNAPSHOT

# mv -f data/testnet/${SNAPSHOT}/db devnet/
