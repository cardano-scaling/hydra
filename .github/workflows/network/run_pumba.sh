#!/usr/bin/env bash

if ! command -v jq &> /dev/null
then
    echo "jq is required for this script ~ install it."
    exit 1
fi

target_peer=$1

percent=$2

peers_info_json=$3

target_node_name=$(echo "$peers_info_json" | jq -r ".$target_peer.node_name")

# Build Pumba netem command
unselected_networks=$(echo "$peers_info_json" | jq -r --arg selected_peer "$target_peer" '
    to_entries[] | select(.key != $selected_peer) | .value.network
')

nix_command="nix run github:noonio/pumba/noon/add-flake -- -l debug --random netem --duration 20m"

while IFS= read -r network; do
    nix_command+=" --target $network"
done <<< "$unselected_networks"

nix_command+=" loss --percent \"$percent\" \"re2:$target_node_name\" &"

echo "$nix_command"

# Run Pumba netem command
eval "$nix_command"