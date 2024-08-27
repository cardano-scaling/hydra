#!/usr/bin/env bash

if ! command -v jq &> /dev/null
then
    echo "jq is required for this script ~ install it."
    exit 1
fi

extract_hydra_node_info_json() {
    local node_name=$1

    local container_name="demo-$node_name-1"

    local network=$(docker inspect "$container_name" | jq '.[0].NetworkSettings.Networks.demo_hydra_net.IPAMConfig.IPv4Address' | tr -d '"')

    local command_list=$(docker inspect "$container_name" | jq '.[0].Config.Cmd')

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
    echo "{\"node_name\": \"$node_name\", \"info\": $json, \"network\": \"$network\" }"
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
