#! /bin/bash -xe
# Run hydraw instance

# fail if something goes wrong
set -e

docker-compose --profile hydraw up -d
