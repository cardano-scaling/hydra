#!/bin/sh
# Setup project

# fail if something goes wrong
set -e

echo "Enter [profile]: "
profile=
while [[ $profile = "" ]]; do
   read profile
done

echo "Enter [key_name]: "
key_name=
while [[ $key_name = "" ]]; do
   read key_name
done

echo "Enter [gh_account]: "
gh_account=
while [[ $gh_account = "" ]]; do
   read gh_account
done

test -f terraform.tfvars ||
    cat << EOF >terraform.tfvars
profile    = "$profile"
key_name   = "$key_name"
gh_account = "$gh_account"
EOF

test -f terraform.tfvars && setup/env.sh

test -f terraform.tfvars && setup/credentials.sh
