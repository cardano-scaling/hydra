#!/usr/bin/env bash

DOCS_DIR="docs/*.md"

DOCUMENTS=$(git ls-files $DOCS_DIR)

for MD in $DOCUMENTS; do
    echo "enriching: $MD"

    PLACEHOLDER="<DocumentMetadata />"
    sed -i "s|$PLACEHOLDER|<DocumentMetadata path=\"$MD\" />|g" $MD

done

PLACEHOLDER="<SiteMetadata />"
REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local docs)
sed -i "s|$PLACEHOLDER|$REPLACEMENT|g" 'docs/docusaurus.config.js'
