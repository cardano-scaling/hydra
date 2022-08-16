#!/usr/bin/env bash

PLACEHOLDER="<SiteMetadata />"
REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local docs)
sed -i "s|$PLACEHOLDER|$REPLACEMENT|g" 'docusaurus.config.js'
