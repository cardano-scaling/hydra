#!/bin/bash
# SCP files to/from a given instance
# This is just a tiny shell around `gcloud compute scp` adding PROJECT and using
# GOOGLE_APPLICATION_CREDENTIALS file if it is set rather than current login

[ -z "${GOOGLE_APPLICATION_CREDENTIALS}" ] && { echo "please set env variable GOOGLE_APPLICATION_CREDENTIALS"; exit 1; }

gcloud auth activate-service-account --key-file=${GOOGLE_APPLICATION_CREDENTIALS}

PROJECT=${PROJECT:-$(terraform output -raw project)}

exec gcloud "--project=${PROJECT}" compute scp "$@"
