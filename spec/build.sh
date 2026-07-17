#!/usr/bin/env bash
# Build the Hydra specification PDF via Agda (typecheck) + Typst (render).
#
# Stage 1: `agda` typechecks the literate-Typst source tree (fails on type error,
#          preserving the machine-checked property).
# Stage 2: `typst compile` renders the same .lagda.typ tree in place (Typst reads
#          the literate files directly; code fences render as raw blocks, see
#          template.typ for the hidden/visible idiom) to _build/hydra-spec.pdf.
set -euo pipefail
cd "$(dirname "$0")"

SRC=src
PDF=_build/hydra-spec.pdf
ENTRY="$SRC/Hydra/Protocol/Main.lagda.typ"

# Stage 1: typecheck, then the Agda↔Typst reference consistency lint (W6) and the C3 trust-ledger drift check.
agda "$ENTRY"
bash check-refs.sh
bash check-trust-ledger.sh

# Stage 2: render in place. --root=src so the root-relative imports (/template.typ,
# /macros.typ, /short.bib, /agda.sublime-syntax) resolve; the .lagda.typ files include
# each other by name, so no staging/extension-stripping is needed.
#
# JuliaMono (the code font, see template.typ) is not vendored: nix provides it
# (nixpkgs `julia-mono`) via JULIAMONO_FONT_DIR, exported by both `nix build .#spec`
# and the dev shell. Hard-error when unset: typst only warns on a missing font
# family and would silently render code blocks with a fallback font.
mkdir -p "$(dirname "$PDF")"
# Build via a temp file and rename into place ATOMICALLY: typst
# truncate-and-writes its output, and a PDF viewer auto-reloading on change
# (okular etc.) can catch a torn mid-write file and sit on a broken parse.
#
# NB typst >= 0.14.1 required: 0.14.0 emits the PDF named-destination name
# tree unsorted (typst#7248), which silently kills every internal section
# link in viewers that binary-search the tree per spec (pdf.js, PDFium,
# macOS Preview; poppler tolerates it). The nix dev shell and nix build
# provide a fixed typst (see nix/hydra/spec.nix).
TMP="$(dirname "$PDF")/.$(basename "$PDF" .pdf).tmp.pdf"
typst compile --ignore-system-fonts --root "$SRC" \
  --font-path "${JULIAMONO_FONT_DIR:?not set: use the nix dev shell (or nix build .#spec), or point it at a directory with JuliaMono-*.ttf}" \
  --package-cache-path typst-packages "$ENTRY" "$TMP"


# Stage 3: stamp invisible hover-tooltips (definitions of the notation symbols)
# over the prose math; see annotate-notation.py. The rendered pages are
# pixel-identical; viewers with annotation popups (okular, pdf.js, Acrobat)
# show the definition on hover.
#
# ANNOTATE_NOTATION=skip skips the stage: `nix build .#spec` runs it as a
# SEPARATE seconds-long derivation (nix/hydra/spec.nix), so the minutes-long
# Agda+Typst build does not share a build window with the python closure - a
# busy builder's mid-build auto-GC (observed on the darwin CI builders) could
# otherwise collect the late-used python environment out from under this step.
if [ "${ANNOTATE_NOTATION:-}" = "skip" ]; then
  echo "ANNOTATE_NOTATION=skip: leaving the PDF without notation tooltips (postprocess elsewhere)"
else
  python3 annotate-notation.py "$TMP" "$TMP.annotated"
  mv -f "$TMP.annotated" "$TMP"
fi
mv -f "$TMP" "$PDF"
echo "Wrote $PDF"
