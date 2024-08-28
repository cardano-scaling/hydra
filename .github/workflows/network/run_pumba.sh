#!/usr/bin/env bash

target_node_name=$1

percent=$2

rest_node_names=$3

# Build Pumba netem command
nix_command="nix run github:noonio/pumba/noon/add-flake -- -l debug --random netem --duration 20m"

while IFS= read -r network; do
    nix_command+=" --target $network"
done <<< "$rest_node_names"

nix_command+=" loss --percent \"$percent\" \"re2:$target_node_name\" &"

echo "$nix_command"

# Run Pumba netem command
eval "$nix_command"