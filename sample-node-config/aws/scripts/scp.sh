#!/bin/bash
# SCP files to/from a given instance

# fail if something goes wrong
set -e

[ -z "${AWS_PROFILE}" ] && { echo "please set env variable AWS_PROFILE"; exit 1; }

[ $# -eq 1 ] || { echo "requires an argument 'file path'"; exit 1 ; }

FILE_PATH=$1
CONNECT_AS=${2:-'ubuntu'}
KEY_PAIR_LOCATION=${3:-'./env/personal.pem'}

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Key==`Hydraw`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | jq '.[2]' | tr -d '"')

scp -i $KEY_PAIR_LOCATION $FILE_PATH $CONNECT_AS@$INSTANCE_DNS:
