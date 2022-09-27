#!/bin/bash
# SCP files to/from a given instance

# fail if something goes wrong
set -e

[ -z "${AWS_PROFILE}" ] && { echo "please set env variable AWS_PROFILE"; exit 1; }

[ $# -eq 1 ] || { echo "requires an argument 'file path'"; exit 1 ; }

FILE_PATH=$1
CONNECT_AS=${2:-'ubuntu'}
INSTANCE_TAG=$(cat variables.tf | grep -o -P '(?<=tag).*' | grep -o -P '(?<=").*(?=")')
KEY_PAIR_LOCATION=$(cat variables.tf | grep -o -P '(?<=private_key).*' | grep -o -P '(?<=").*(?=")')

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Value==`'$INSTANCE_TAG'`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | grep $INSTANCE_TAG | jq '.[2]' | tr -d '"')

scp -i $KEY_PAIR_LOCATION $FILE_PATH $CONNECT_AS@$INSTANCE_DNS:
