#!/usr/bin/env bash

target_node_name=$1

percent=$2

rest_node_names=$3

# Build Pumba netem command
# Note: We leave it for 20 minutes; but really it's effectively unlimited. We don't
# expect any of our tests to run longer than that.
nix_command="nix run github:noonio/pumba/noon/add-flake -- -l debug netem --duration 20m"

while IFS= read -r network; do
    nix_command+=" --target $network"
done <<< "$rest_node_names"

nix_command+=" loss --percent \"$percent\" \"re2:$target_node_name\" &"

echo "$nix_command"

# Run Pumba netem command
eval "$nix_command"
