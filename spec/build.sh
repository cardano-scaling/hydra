#/usr/bin/env bash

set -eo pipefail

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

# Concat sources
cat macros.md intro.md overview.md prel.md > all.md

# TODO: move more into defaults file?
pandoc macros.md intro.md overview.md prel.md \
      --metadata-file meta.yaml \
      -d filters.yaml \
      --filter pandoc-crossref \
      --citeproc \
      --pdf-engine=xelatex \
      -o $out/hydra-spec.pdf

pandoc macros.md intro.md overview.md prel.md \
      --metadata-file meta.yaml \
      -d filters.yaml \
      --filter ./fix-figures.hs \
      --extract-media img \
      --strip-comments \
      --filter pandoc-crossref \
      --citeproc \
      --to markdown \
      -o converted.md

# Copy extracted images
cp -a img $out/

# The raw_html <img> paths are not supported well by docusaurus. We do convert
# them to markdown-style images but retain html for the <figure>
cat converted.md | sed 's|<img src="\(.*\)" />|\n![](\1)\n|' > replaced-images.md

# Remove macros.md again / everything until first headline
# TODO: use a pandoc filter for this
awk '/^#/{f=1}f' replaced-images.md > removed-macros.md

# Demote all headlines by one level
# TODO: use a pandoc filter for this
cat removed-macros.md | sed 's/^#/##/g' > demoted.md

# TODO: avoid duplication on document title
echo "# Hydra HeadV1 Specification: Coordinated Head protocol" > $out/hydra-spec.md
cat demoted.md >> $out/hydra-spec.md

echo ${out}
