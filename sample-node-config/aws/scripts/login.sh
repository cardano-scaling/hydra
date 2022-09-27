#!/bin/sh
# Connect to remote VM using creds/names given by args

# fail if something goes wrong
set -e

[ -z "${AWS_PROFILE}" ] && { echo "please set env variable AWS_PROFILE"; exit 1; }

CONNECT_AS=${1:-'ubuntu'}

KEY_NAME=$(cat terraform.tfvars | grep -o -P '(?<=key_name).*' | grep -o -P '(?<=").*(?=")')
INSTANCE_TAG="hydraw-$KEY_NAME"
KEY_PAIR_LOCATION="./env/$KEY_NAME.pem"

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Value==`'$INSTANCE_TAG'`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | grep $INSTANCE_TAG | jq '.[2]' | tr -d '"')

ssh -i $KEY_PAIR_LOCATION $CONNECT_AS@$INSTANCE_DNS
