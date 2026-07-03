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
typst compile --ignore-system-fonts --root "$SRC" \
  --font-path "${JULIAMONO_FONT_DIR:?not set: use the nix dev shell (or nix build .#spec), or point it at a directory with JuliaMono-*.ttf}" \
  --package-cache-path typst-packages "$ENTRY" "$PDF"
echo "Wrote $PDF"
