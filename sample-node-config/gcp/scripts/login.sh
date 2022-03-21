#!/bin/bash
# Connect to remote VM using creds/names given by $1

[ -z "${GOOGLE_APPLICATION_CREDENTIALS}" ] && { echo "please set env variable GOOGLE_APPLICATION_CREDENTIALS"; exit 1; }

[ $# -eq 1 ] || { echo "requires an argument 'user@machine-name'"; exit 1 ; }

gcloud auth activate-service-account --key-file=${GOOGLE_APPLICATION_CREDENTIALS}

CONNECT_AS=$1
PROJECT=${PROJECT:-$(terraform output -raw project)}


gcloud "--project=${PROJECT}" compute ssh "$CONNECT_AS" -- -A \
       -o StreamLocalBindUnlink=yes \
       -o ForwardAgent=yes \
       -R /run/user/1001/gnupg/S.gpg-agent:/Users/arnaud/.gnupg/S.gpg-agent.extra \
       -L 8000:localhost:8000 \
       -L 9090:localhost:9090
