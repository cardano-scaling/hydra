_: {

  perSystem = { pkgs, hsPkgs, lib, ... }:
    let
      allComponents = x:
        [ x.components.library ]
        ++ lib.concatMap
          (y: builtins.attrValues x.components."${y}")
          [ "benchmarks" "exes" "sublibs" "tests" ];

    in
    {


      coding.standards.hydra = {
        enable = true;
        haskellPackages = with hsPkgs; builtins.concatMap allComponents [
          hydra-cardano-api
          hydra-chain-observer
          hydra-cluster
          hydra-node
          hydra-tx
          hydra-prelude
          hydra-plutus
          hydra-plutus-extras
          hydra-test-utils
          hydra-tui
          hydraw
          visualize-logs
        ];
        inherit (pkgs) weeder;
        haskellType = "haskell.nix";
      };

    };
}
