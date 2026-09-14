# The local Haskell packages built from this repo. Shared by the modules that
# need to treat "our" packages differently from upstream dependencies: the
# -Werror ghcOptions (project.nix), the build checks (werror-checks.nix), and
# the coding-standards weeder list (coding-standards.nix).
_: {
  perSystem = _: {
    _module.args.localHaskellPackageNames = [
      "cborg-generic-tagged"
      "contra-tracer-json"
      "event-sourcing"
      "head-state-viewer"
      "hydra-agda"
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
      "io-classes-labelled"
      "persistent-queue"
      "secret"
      "test-network-ports"
    ];

    # The subset with a `test-suite` stanza, i.e. the packages for which
    # `hsPkgs.<pkg>.checks.tests` exists. A literal list rather than something
    # derived from `hsPkgs`, so that project.nix can consume it without a
    # `_module.args` cycle.
    _module.args.testSuitePackageNames = [
      "hydra-agda"
      "hydra-chain-observer"
      "hydra-cluster"
      "hydra-node"
      "hydra-plutus"
      "hydra-plutus-extras"
      "hydra-tui"
      "hydra-tx"
    ];
  };
}
