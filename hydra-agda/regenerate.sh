#!/usr/bin/env bash
# Regenerate the MAlonzo-extracted Haskell under generated/ from the Agda reference checker.
# Run inside the default dev shell (`nix develop`), which provides `agda` (with the spec
# libraries) and GHC. After running, review the diff and update src/Hydra/Agda/Reference.hs if
# any mangled MAlonzo names (T_Ops_*, C_Ops'46'constructor_*, d_closeRef'7495'_*) changed.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/.." && pwd)"
rm -rf "$here/generated/MAlonzo"
cd "$repo/spec"
agda --compile --no-main --ghc-dont-call-ghc --compile-dir="$here/generated" \
  src/Hydra/Protocol/Reference.agda

# Stamp `-w` on every generated module: `just lint` builds with command-line
# `-Werror -Wall`, which would otherwise escalate the extractor's harmless warnings
# (name shadowing, unused imports, …) to errors. A file-level OPTIONS_GHC pragma is
# appended after the command-line flags, so `-w` wins. The whole generated/ tree is
# also excluded from treefmt (see nix/coding-standards.nix).
find "$here/generated" -name '*.hs' -print0 | while IFS= read -r -d '' f; do
  chmod u+w "$f"   # stdlib runtime files are copied read-only from the nix store
  if ! head -1 "$f" | grep -q -- '-w'; then
    printf '{-# OPTIONS_GHC -w #-}\n%s' "$(cat "$f")" > "$f"
  fi
done
echo "Regenerated $here/generated/MAlonzo"
