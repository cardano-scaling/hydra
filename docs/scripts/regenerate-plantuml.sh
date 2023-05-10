#!/usr/bin/env bash

# Regenerate all plantuml drawings
set -eo pipefail

changes=()
for puml in $(find -name *.puml); do
    plantuml -Tsvg ${puml}

    # Collect changes in all directories holding .puml files
    # NOTE: The .puml file does include a file name, so it does not directly
    # correspond to the same basename with .svg extension
    change=$(git status --porcelain $(dirname ${puml}))
    if [ -n "${change}" ]; then
        changes+=("${change}")
    fi
done

if [[ ${#changes[@]} -gt 0 ]]; then
    echo "WARNING: Uncommitted changes after regenerating plantuml drawings"
    printf "%s\n" "${changes[@]}"
fi
