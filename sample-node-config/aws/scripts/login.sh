#!/bin/bash
# Connect to remote VM using creds/names given by args

# fail if something goes wrong
set -e

[ -z "${AWS_PROFILE}" ] && { echo "please set env variable AWS_PROFILE"; exit 1; }

CONNECT_AS=${1:-'ubuntu'}
KEY_PAIR_LOCATION=${2:-'./env/personal.pem'}

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Key==`Hydraw`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | jq '.[2]' | tr -d '"')

ssh -i $KEY_PAIR_LOCATION $CONNECT_AS@$INSTANCE_DNS
