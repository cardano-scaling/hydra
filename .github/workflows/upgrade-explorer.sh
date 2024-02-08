#!/usr/bin/env bash
set -e

# Function to get the image hash of a running container
get_container_hash() {
    local container_name=$1
    docker inspect --format='{{.Image}}' "$container_name"
}
# Function to get the image hash of the $VERSION in the registry
get_registry_hash() {
    local image_name=$1
    docker pull "$1" >/dev/null 2>&1
    docker inspect --format='{{index .RepoDigests 0}}' "$image_name" | cut -d '@' -f2
}
# Check if the running container exists
if [ "$(docker ps -q -f name=hydra-explorer)" ]; then
    # Get the image hash of the running container
    current_hash=$(get_container_hash hydra-explorer)
    # Get the image hash of the $VERSION in the registry
    registry_hash=$(get_registry_hash ghcr.io/input-output-hk/hydra-explorer:$VERSION)
    # Compare the hashes
    if [ "$current_hash" != "$registry_hash" ]; then
        echo "Updating container..."
        # Stop and remove the running container
        docker stop hydra-explorer >/dev/null 2>&1
        docker rm hydra-explorer >/dev/null 2>&1
        # Pull and run the new $VERSION image
        docker run -d --name hydra-explorer \
          --restart always \
          --pull always \
          -v /srv/var/cardano/state-preview:/preview \
          -p 9090:9090 \
          ghcr.io/input-output-hk/hydra-explorer:$VERSION \
          --node-socket /preview/node.socket \
          ${NETWORK}
    else
        echo "Container is already up to date."
    fi
else
    echo "Container is not running."
fi
