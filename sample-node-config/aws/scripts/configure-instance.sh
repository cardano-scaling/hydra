#! /bin/bash -xe
# Configure instance for user ubuntu

get_rid_of_apt_service() {
  echo 'Terminating system apt updates so that we can install stuff'
  sudo systemctl stop apt-daily.service
  sudo systemctl kill --kill-who=all apt-daily.service

  # wait until `apt-get updated` has been killed
  while ! (systemctl list-units --all apt-daily.service | egrep -q '(dead|failed)')
  do
    sleep 1;
  done
}

# fail if something goes wrong
set -e

get_rid_of_apt_service

echo 'Updating system'
sudo apt-get update -y
sudo apt-get upgrade -y

echo 'Installing jq'
sudo apt-get install jq -y

echo 'Installing docker' # https://docs.docker.com/engine/install/ubuntu/
curl -fsSL https://get.docker.com -o ~/scripts/get-docker.sh
sh ~/scripts/get-docker.sh
sudo usermod -aG docker ubuntu                    # to add myself to docker group

echo 'Installing docker-compose' # https://gist.github.com/npearce/6f3c7826c7499587f00957fee62f8ee9
sudo curl -L https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose     # to allow docker group users to execute it
sudo chgrp docker /usr/local/bin/docker-compose # to give docker-compose to docker group,

echo -e "\nsource ~/.bash_env" >> ~/.bashrc
echo -e "\nsource ~/.bash_profile" >> ~/.bashrc
