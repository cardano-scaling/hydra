#!/bin/bash

# Update an upstream repository if there's any new file updates.

STATUS=$(git status -s)
if [ -z "$STATUS" ]; then
    echo "Nothing to update."
else
    git config --global user.email "noreply@github.com"
    git config --global user.name "Automated Bot"
    git add .
    git commit -m "Upload configuration from: $1"
    git push --set-upstream origin "master"
fi
