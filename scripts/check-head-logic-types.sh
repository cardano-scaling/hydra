#!/usr/bin/env bash
set -euo pipefail

# Use Cabal's local library by default; Nix supplies an already built GHC env.
if [[ "${1:-}" == --ghc ]]; then
  compiler=(ghc -package hydra-node -package crem)
  fixtures="$2"
else
  cabal build hydra-node:lib:hydra-node
  # Cabal selects package IDs; -package can expose another installed Crem version.
  compiler=(cabal exec -- ghc)
  fixtures="$(dirname "$0")/../hydra-node/typecheck"
fi

typecheck_dir=$(mktemp -d)
trap 'rm -rf "$typecheck_dir"' EXIT
flags=(-fno-code -fforce-recomp -fdiagnostics-color=never -XGHC2021 -outputdir "$typecheck_dir")

"${compiler[@]}" "${flags[@]}" "$fixtures/Allowed.hs"

reject() {
  local source="$1"
  shift
  if "${compiler[@]}" "${flags[@]}" "$fixtures/$source.hs" >"$typecheck_dir/$source.log" 2>&1; then
    echo "FAIL: $source unexpectedly compiles" >&2
    exit 1
  fi
  for expected in "$@"; do
    if ! grep -Fq "$expected" "$typecheck_dir/$source.log"; then
      cat "$typecheck_dir/$source.log" >&2
      echo "FAIL: $source fails for an unexpected reason (missing $expected)" >&2
      exit 1
    fi
  done
  echo "$source: rejected for the expected reason"
}

reject SnapshotRestart 'No instance for' 'AllowedTransition' 'CollectingSignatures' 'RequestSent'
reject DecommitReplacement 'No instance for' 'AllowedTransition' 'AwaitingSettlement' 'DecommitPending'
reject CertificateAsCandidate 'VerifiedCandidate' 'VerifiedSnapshot' "Couldn't match type"
