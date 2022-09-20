#! /bin/bash -xe
# Configure instance for user ubuntu

# fail if something goes wrong
set -e

echo 'updating system'
sudo apt update -y

echo 'installing jq'
sudo apt-get install jq -y

echo 'installing docker' # https://docs.docker.com/engine/install/ubuntu/
curl -fsSL https://get.docker.com -o get-docker.sh
sh ./get-docker.sh
sudo usermod -aG docker ubuntu                    # to add myself to docker group

echo 'installing docker-compose' # https://gist.github.com/npearce/6f3c7826c7499587f00957fee62f8ee9
sudo curl -L https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose     # to allow docker group users to execute it
sudo chgrp docker /usr/local/bin/docker-compose # to give docker-compose to docker group,

cd /home/ubuntu
touch .bashrc
echo "alias reload='source .bashrc'" >> .bashrc
echo "alias logs='cat /var/log/cloud-init.log'" >> .bashrc
echo "alias udlogs='cat /var/log/user-data.log'" >> .bashrc
echo "alias g=git" >> .bashrc
echo "alias d=docker" >> .bashrc
echo "alias dc=docker-compose" >> .bashrc
echo "alias fuel='cd ~ && ./create-marker-utxo.sh'" >> .bashrc
echo "alias up='cd ~ && ./hydraw-up.sh'" >> .bashrc
echo "alias down='cd ~ && ./hydraw-down.sh'" >> .bashrc
echo "alias tui='cd ~ && ./run-tui.sh'" >> .bashrc
echo "alias sync='docker exec -it ubuntu-cardano-node-1 cardano-cli query tip --testnet-magic=2'" >> .bashrc
