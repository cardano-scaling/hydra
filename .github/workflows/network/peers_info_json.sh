#!/usr/bin/env bash

if ! command -v yq &> /dev/null || ! command -v jq &> /dev/null
then
    echo "yq and jq are required for this script ~ install them."
    exit 1
fi

compose_file=$1

extract_hydra_node_info_json() {
    local node_name=$1

    local port=$(yq eval ".services.$node_name.ports[0]" $compose_file | awk -F: '{print $1}' | tr -d '"')

    local network=$(yq eval ".services.$node_name.networks.hydra_net.ipv4_address" $compose_file | tr -d '"')

    local command_list=$(yq eval ".services.$node_name.command" $compose_file)

    local json=$(
        echo "$command_list" | jq -r '
            . as $arr |
            reduce range(0; ($arr | length) / 2) as $i (
                {};
                . + {($arr[2 * $i]): $arr[2 * $i + 1]}
            )
            | with_entries(
                select(
                    .key == "--node-id" or
                    .key == "--api-host" or
                    .key == "--host" or
                    .key == "--hydra-signing-key" or
                    .key == "--cardano-signing-key" or
                    .key == "--ledger-protocol-parameters" or
                    .key == "--testnet-magic" or
                    .key == "--persistence-dir"
                )
            )
            | with_entries(
                .key |= sub("^--"; "") |  # Remove --
                .key |= gsub("-"; "_")    # Replace - with _
            )'
    )
    echo "{\"node_name\": \"$node_name\", \"info\": $json, \"port\": \"$port\", \"network\": \"$network\" }"
}

peers_info_json() {
    echo "{
        \"alice\": $(extract_hydra_node_info_json "hydra-node-1"),
        \"bob\": $(extract_hydra_node_info_json "hydra-node-2"),
        \"carol\": $(extract_hydra_node_info_json "hydra-node-3")
    }"
}

json_result=$(echo $(peers_info_json) | jq -c)

if [[ -z "$json_result" ]]; then
  echo "Error: json_result is empty."
  exit 1
fi

echo $json_result
