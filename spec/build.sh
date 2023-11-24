#/usr/bin/env bash

set -exo pipefail

out=$1
if [ -z ${out} ]; then
     out=$(dirname $0)
fi
out=$(realpath ${out})

>&2 echo "Creating specification in ${out}"

mkdir -p $out

# TODO: use a Makefile?
# TODO: no in-place processing to improve out-of-nix usage
# TODO: avoid repetition of source files and/or have a single source md

# Preprocess mermaid blocks into SVGs
# FIXME: This requires nix build --no-sandbox as it connects seemingly to localhost
mmdc -i overview.md -o overview.resolved.md

pandoc \
      macros.md \
      intro.md \
      overview.resolved.md \
      prel.md \
      --metadata-file meta.yaml \
      --filter pandoc-crossref \
      --citeproc \
      --pdf-engine=xelatex \
      -o $out/hydra-spec.pdf

pandoc \
      macros.md \
      intro.md \
      overview.resolved.md \
      prel.md \
      --metadata-file meta.yaml \
      --strip-comments \
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
