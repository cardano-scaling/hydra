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
mkdir -p "$(dirname "$PDF")"
typst compile --ignore-system-fonts --root "$SRC" --font-path "$SRC/fonts" \
  --package-cache-path typst-packages "$ENTRY" "$PDF"
echo "Wrote $PDF"
