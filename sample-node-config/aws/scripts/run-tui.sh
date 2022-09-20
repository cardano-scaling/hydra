#! /bin/bash -xe
# Run hydra tui

# fail if something goes wrong
set -e

docker-compose --profile tui run hydra-tui
