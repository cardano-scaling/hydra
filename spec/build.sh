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
      intro.md \
      prel.md \
      --metadata-file meta.yaml \
      --filter pandoc-crossref \
      --citeproc \
      --pdf-engine=xelatex \
      -o $out/hydra-spec.pdf

pandoc \
      macros.md \
      intro.md \
      prel.md \
      --metadata-file meta.yaml \
      --filter pandoc-crossref \
      --citeproc \
      -o converted.md

# Remove macros.md again / everything until first headline
awk '/^#/{f=1}f' converted.md > removed-macros.md

# Demote all headlines by one level
cat removed-macros.md | sed 's/^#/##/g' > demoted.md


# TODO: avoid duplication on document title
echo "# Hydra HeadV1 Specification: Coordinated Head protocol" > $out/hydra-spec.md
cat demoted.md >> $out/hydra-spec.md

echo ${out}
