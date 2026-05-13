alias c := check
alias t := test
alias l := lint

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
test PKG="all" PATTERN="/":
  cabal test {{PKG}} --test-options='--match "{{PATTERN}}"'

# build with -Werror and strict linting flags.
lint PKG="all":
  cabal build {{PKG}} \
    --ghc-options="-Werror \
      -Wall \
      -Wcompat \
      -Widentities \
      -Wincomplete-record-updates \
      -Wincomplete-uni-patterns \
      -Wmissing-deriving-strategies \
      -Wredundant-constraints \
      -Wunused-packages"
