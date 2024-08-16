#!/usr/bin/env bash

set -ex

LOG_FILE_BOB="demo/devnet/persistence/bob/server-output"
LOG_FILE_CAROL="demo/devnet/persistence/carol/server-output"
JQ_EXPRESSION='select(.tag == "PeerDisconnected" and .peer == "1")'

check_log() {
    local log_file=$1
    jq "$JQ_EXPRESSION" "$log_file" | grep -q .
}

bob_ready=false
carol_ready=false

while true; do
    if ! $bob_ready && check_log "$LOG_FILE_BOB"; then
        echo "Match found in Bob's log file!"
        bob_ready=true
    fi

    if ! $carol_ready && check_log "$LOG_FILE_CAROL"; then
        echo "Match found in Carol's log file!"
        carol_ready=true
    fi

    if $bob_ready && $carol_ready; then
        echo "Both conditions met!"
        break
    fi

    sleep 5
done
