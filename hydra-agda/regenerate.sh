#!/usr/bin/env bash
# Regenerate the MAlonzo-extracted Haskell under generated/ from the Agda reference checkers.
#
# Run inside the spec dev shell (`nix develop .#spec`), which provides `agda` with the spec libraries
# (the default shell deliberately does not carry the spec toolchain, see nix/hydra/shell.nix).
# The shims in src/ bind the extraction by the stable names the Agda sources fix with
# `COMPILE GHC … as …` (see the "extraction surface" section of Reference.agda), so a regeneration
# needs no follow-up edit there; a name that disappears is a compile error in the shim.
#
# With an argument, extract into that directory instead of ./generated. `checks.hydra-agda-generated`
# (nix/hydra/spec.nix) uses it to re-extract hermetically into $TMPDIR and diff against the committed
# tree, so the drift gate runs this script rather than a second copy of it.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/.." && pwd)"
out="${1:-$here/generated}"

mkdir -p "$out"
rm -rf "$out/MAlonzo"
cd "$repo/spec"
# On-chain validator reference (Reference.agda) and off-chain HeadLogic reference
# (OffChainReference.agda). Both extract into the same generated/ tree (shared MAlonzo runtime).
agda --compile --no-main --ghc-dont-call-ghc --compile-dir="$out" \
  src/Hydra/Protocol/Reference.agda
agda --compile --no-main --ghc-dont-call-ghc --compile-dir="$out" \
  src/Hydra/Protocol/OffChainReference.agda

# Stamp `-w` on every generated module: `just lint` builds with command-line
# `-Werror -Wall`, which would otherwise escalate the extractor's harmless warnings
# (name shadowing, unused imports, …) to errors. A file-level OPTIONS_GHC pragma is
# appended after the command-line flags, so `-w` wins. The whole generated/ tree is
# also excluded from treefmt (see nix/coding-standards.nix).
#
# `cat` the file rather than interpolating it into printf: command substitution strips trailing
# newlines, so `printf '…\n%s' "$(cat "$f")"` left every generated file without its final newline,
# which .editorconfig then wants to add back. Any editor or hook that obliged made the committed
# tree differ from a fresh extraction and failed the drift gate for no semantic reason.
find "$out" -name '*.hs' -print0 | while IFS= read -r -d '' f; do
  chmod u+w "$f" # stdlib runtime files are copied read-only from the nix store
  if ! head -1 "$f" | grep -qxF -- '{-# OPTIONS_GHC -w #-}'; then
    { printf '{-# OPTIONS_GHC -w #-}\n'; cat "$f"; } > "$f.stamped"
    mv -f "$f.stamped" "$f"
  fi
done
# The cabal library lists the generated modules by hand, so a regeneration that adds or drops one
# would otherwise pass the drift check and surface later as a confusing "module not found" during the
# build. Compare the two here, where the answer is obvious. Scoped to the library stanza: the
# test-suite has an other-modules block of its own.
generated_modules=$(
  cd "$out" && find . -name '*.hs' | sed 's|^\./||; s|\.hs$||; s|/|.|g' | sort
)
listed_modules=$(
  awk '/^library$/ {inlib = 1}
       inlib && /^[a-z]/ && !/^library$/ && !/^ / {inlib = 0}
       inlib && /^  other-modules:/ {inmods = 1; next}
       inmods && /^    [A-Za-z]/ {print $1; next}
       inmods {inmods = 0}' "$here/hydra-agda.cabal" | sort
)
if [ "$generated_modules" != "$listed_modules" ]; then
  echo "regenerate.sh: hydra-agda.cabal's library other-modules do not match $out (- cabal, + generated):"
  diff <(printf '%s\n' "$listed_modules") <(printf '%s\n' "$generated_modules") || true
  echo "Update the other-modules list in hydra-agda/hydra-agda.cabal."
  exit 1
fi

echo "Regenerated $out/MAlonzo ($(printf '%s\n' "$generated_modules" | wc -l) modules, matching hydra-agda.cabal)"
