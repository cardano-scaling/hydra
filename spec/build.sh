#!/usr/bin/env bash
# Build the Hydra specification PDF via Agda (typecheck) + Typst (render).
#
# Stage 1: `agda` typechecks the literate-Typst source tree (fails on type error,
#          preserving the machine-checked property).
# Stage 2: stage sources into _build/typst, stripping the .lagda extension so the
#          literate files become plain .typ Typst inputs (code fences render as raw
#          blocks; see template.typ for the hidden/visible idiom).
# Stage 3: `typst compile` produces _build/hydra-spec.pdf.
set -euo pipefail
cd "$(dirname "$0")"

SRC=src
OUT=_build/typst
PDF=_build/hydra-spec.pdf
ENTRY=Hydra/Protocol/Main

# Stage 1: typecheck, then the Agda↔Typst reference consistency lint (W6).
agda "$SRC/$ENTRY.lagda.typ"
bash check-refs.sh

# Stage 2: stage typst sources
rm -rf "$OUT"
mkdir -p "$OUT"
cp "$SRC"/*.typ "$OUT"/
cp "$SRC"/short.bib "$OUT"/
cp "$SRC"/agda.sublime-syntax "$OUT"/
while IFS= read -r f; do
  rel=${f#"$SRC"/}
  dest="$OUT/${rel%.lagda.typ}.typ"
  mkdir -p "$(dirname "$dest")"
  cp "$f" "$dest"
done < <(find "$SRC" -name '*.lagda.typ')
mkdir -p "$OUT/Hydra/Protocol/Figures"
cp "$SRC"/Hydra/Protocol/Figures/*.svg "$OUT/Hydra/Protocol/Figures/"

# Stage 3: compile
mkdir -p "$(dirname "$PDF")"
typst compile --ignore-system-fonts --root "$OUT" --font-path "$SRC/fonts" \
  --package-cache-path typst-packages "$OUT/$ENTRY.typ" "$PDF"
echo "Wrote $PDF"
