alias c := check
alias t := test

[private]
default:
  @just --list

# run 'selfci'
ci:
  selfci check --print-output

# run "nix-fast-build" to run the nix checks
check:
  nix-fast-build \
    --flake ".#checks.$(nix eval --impure --raw --expr builtins.currentSystem)" \
    --no-link \
    --skip-cached

# run cabal tests, optionally with some test selection
test ARG1="all" ARG2="/":
  cabal test {{ARG1}} --test-options="--match {{ARG2}}"
