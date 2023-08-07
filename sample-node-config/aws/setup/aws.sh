#! /bin/bash -xe
# Build aws

# fail if something goes wrong
set -e

DIR=aws

test -d $DIR || mkdir $DIR

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

echo "creating key pair $KEY_NAME under profile $AWS_PROFILE"

test -f $DIR/$KEY_NAME.pem || 
    aws --profile=$AWS_PROFILE ec2 create-key-pair \
    --key-name $KEY_NAME \
    --key-type ed25519 \
    --key-format pem  \
    --query 'KeyMaterial' \
    --output text > $DIR/$KEY_NAME.pem
