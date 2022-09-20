#! /bin/bash -xe
# Take down hydraw instance

# fail if something goes wrong
set -e

docker-compose --profile hydraw down
