#!/bin/sh
# Connect to remote VM using creds/names given by args

# fail if something goes wrong
set -e

CONNECT_AS=${1:-'ubuntu'}

AWS_PROFILE=$(cat terraform.tfvars | awk '{
    if (match($0, /^profile[[:space:]]*=[[:space:]]*"([^"]+)"/)) {
        value = substr($0, RSTART + 10, RLENGTH - 10);
        gsub(/^[[:space:]]*=|[[:space:]]+|"/, "", value);
        print value;
    }
}')
KEY_NAME=$(cat terraform.tfvars | awk '{
    if (match($0, /^key_name[[:space:]]*=[[:space:]]*"([^"]+)"/)) {
        value = substr($0, RSTART + 10, RLENGTH - 10);
        gsub(/^[[:space:]]*=|[[:space:]]+|"/, "", value);
        print value;
    }
}')
NETWORK=$(cat terraform.tfvars | awk '{
    if (match($0, /^env[[:space:]]*=[[:space:]]*"([^"]+)"/)) {
        value = substr($0, RSTART + 10, RLENGTH - 10);
        gsub(/^[[:space:]]*=|[[:space:]]+|"/, "", value);
        print value;
    }
}')
INSTANCE_TAG="$KEY_NAME-$NETWORK"
KEY_PAIR_LOCATION="./aws/$KEY_NAME.pem"

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Value==`'$INSTANCE_TAG'`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | grep $INSTANCE_TAG | jq '.[2]' | tr -d '"')

ssh -i $KEY_PAIR_LOCATION $CONNECT_AS@$INSTANCE_DNS
