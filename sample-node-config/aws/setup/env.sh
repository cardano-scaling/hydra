#! /bin/bash -xe
# Build env

# fail if something goes wrong
set -e

AWS_PROFILE=$(cat terraform.tfvars | grep -o -P '(?<=profile).*' | grep -o -P '(?<=").*(?=")')
KEY_NAME=$(cat terraform.tfvars | grep -o -P '(?<=key_name).*' | grep -o -P '(?<=").*(?=")')

echo "creating key pair $KEY_NAME under profile $AWS_PROFILE"

test -f env/$KEY_NAME.pem || 
    aws --profile=$AWS_PROFILE ec2 create-key-pair \
    --key-name $KEY_NAME \
    --key-type ed25519 \
    --key-format pem  \
    --query 'KeyMaterial' \
    --output text > env/$KEY_NAME.pem
