#!/usr/bin/env bash

# Container name
target_node_name=$1

# Valid pumba action to be taken.
action=$2

# command args
args=$3

# List of other hosts (space-separated <ip:port>)
others=$4

# Start build a Pumba command
pumba_command="nix run github:noonio/pumba/noon/add-flake -- -l debug $action"

# Add network targets only if the command is related to netem (like 'loss', 'delay')
if [[ $action == netem* ]]; then
    # Note: We leave it for 20 minutes; but really it's effectively unlimited. We don't
    # expect any of our tests to run longer than that.
    pumba_command+=" --duration 20m"
    for network in $others; do
        pumba_command+=" --target $network"
    done
fi

pumba_command+=" $args"

# Select target container
pumba_command+=" \"re2:$target_node_name\" &"

echo "$pumba_command"

# Run command
eval "$pumba_command"
