#!/usr/bin/env bash

# [note] use 'docs/*.md' for local debugging
DOCS_DIR="docs/*.md"

MARKDOWN_COMMENT="\[\/\/\]:"

DOCUMENTS=$(git ls-files  $DOCS_DIR)

for MD in $DOCUMENTS; do

    PLACEHOLDER="$MARKDOWN_COMMENT{{last-updated-at}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local $MD)
    sed -i '' "s/$PLACEHOLDER/Last updated at: $REPLACEMENT/g" $MD

    PLACEHOLDER="$MARKDOWN_COMMENT{{author-info}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%aN <%ae>' $MD)
    sed -i '' "s/$PLACEHOLDER/Author: $REPLACEMENT/g" $MD

    PLACEHOLDER="$MARKDOWN_COMMENT{{last-translated-at}}"
    REPLACEMENT=$(git --no-pager log -1 --pretty=format:'%ad' --date=local docs/i18n)
    sed -i '' "s/$PLACEHOLDER/Last translated at: $REPLACEMENT/g" $MD

done