#!/usr/bin/env bash

target_peer=$1

peers_info_json=$2

peers_to_watch_json=$(echo "$peers_info_json" | jq -r --arg selected_peer "$target_peer" '
  to_entries 
  | map(select(.key != $selected_peer)) 
  | map(.key)
')

logs_file_json=$(echo "$peers_info_json" | jq --arg selected_peer "$target_peer" '
  to_entries
  | map(select(.key != $selected_peer))
  | map({
    (.key): (.value.info.persistence_dir | sub("^/"; ""))
  })
  | add
')

target_peer_id=$(echo "$peers_info_json" \
    | jq -r --arg selected_peer "$target_peer" '.[$selected_peer].info.node_id')

check_log() {
    local peer=$1
    local peer_log_file=$(echo "$logs_file_json" | jq -r --arg peer "$peer" '.[$peer]')
    if jq --arg target_peer_id $target_peer_id \
        'select(.tag == "PeerDisconnected" and .peer == $target_peer_id)' \
        "demo/$peer_log_file/server-output" | grep -q .; then
        echo "Success for peer: $peer"
        return 0  # Success: log file has output
    else
        echo "Failure for peer: $peer"
        return 1  # Failure: log file is empty or doesn't exist
    fi
}

declare -A peer_ready
for peer in $(echo "$peers_to_watch_json" | jq -r '.[]'); do
    peer_ready[$peer]=false
done

while true; do
    # Assume all are ready until proven otherwise
    all_ready=true

    for peer in "${!peer_ready[@]}"; do
        echo "Checking $peer..."

        if [[ "${peer_ready[$peer]}" == false ]]; then
            check_log "$peer"
            # Check the exit status of the check_log function
            if [[ $? -eq 0 ]]; then
                echo "$peer is now marked as ready."
                peer_ready[$peer]=true
            fi
        fi

        # Check if the current peer is not ready
        if [[ "${peer_ready[$peer]}" == false ]]; then
            # If any peer is not ready, set all_ready to false
            all_ready=false
        fi
    done

    if [[ "$all_ready" == true ]]; then
        echo "All peers are ready!"
        break
    fi

    sleep 5
done