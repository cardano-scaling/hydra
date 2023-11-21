#/usr/bin/env bash

set -eo pipefail

out=$1
if [ -z ${out} ]; then
     out=$(dirname $0)
fi
out=$(realpath ${out})

>&2 echo "Creating specification in ${out}"

mkdir -p $out

pandoc \
      macros.md \
      README.md \
      intro.md \
      prel.md \
      --filter pandoc-crossref \
      --citeproc \
      --pdf-engine=xelatex \
      -o $out/hydra-spec.pdf

pandoc \
      macros.md \
      README.md \
      intro.md \
      prel.md \
      --filter pandoc-crossref \
      --citeproc \
      --to markdown+latex_macros \
      -o $out/hydra-spec.md

echo ${out}
