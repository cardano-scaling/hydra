#!/usr/bin/env bash

DOCS_DIR="docs/*.md"

DOCUMENTS=$(git ls-files $DOCS_DIR)

for MD in $DOCUMENTS; do
    echo "updating: $MD"

    PLACEHOLDER="{{last-updated-at}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local $MD)
    
    echo $(sed "s/$PLACEHOLDER/$REPLACEMENT/g" $MD)

    sed -i "s/$PLACEHOLDER/$REPLACEMENT/g" $MD
    
    PLACEHOLDER="{{relative-time-since}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%cr' $MD)
    sed -i "s/$PLACEHOLDER/$REPLACEMENT/g" $MD

    PLACEHOLDER="{{commit-hash}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%h' $MD)
    sed -i "s/$PLACEHOLDER/$REPLACEMENT/g" $MD

    PLACEHOLDER="{{last-translated-at}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local docs/i18n)
    sed -i "s/$PLACEHOLDER/$REPLACEMENT/g" $MD


done
