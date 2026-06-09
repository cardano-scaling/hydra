# The local Haskell packages built from this repo. Shared by the modules that
# need to treat "our" packages differently from upstream dependencies: the
# -Werror ghcOptions (project.nix), the build checks (werror-checks.nix), and
# the coding-standards weeder list (coding-standards.nix).
_: {
  perSystem = _: {
    _module.args.localHaskellPackageNames = [
      "hydra-cardano-api"
      "hydra-chain-observer"
      "hydra-cluster"
      "hydra-node"
      "hydra-plutus"
      "hydra-plutus-extras"
      "hydra-prelude"
      "hydra-test-utils"
      "hydra-tx"
      "hydra-tui"
      "hydraw"
    ];
  };
}
