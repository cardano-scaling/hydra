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
        # GHC 9.12 needs a source-repository-package for proto-lens (the
        # Hackage release doesn't compile on 9.12); the no-srp check would
        # otherwise fail. Disable until proto-lens 0.7.2+ is on Hackage.
        srp-check = false;
      };

      # weeder 2.10.0 segfaults on HIE files produced by GHC 9.12.4, so the
      # weeder check would crash even though our code is clean. Disable until
      # weeder ships a release that handles 9.12 HIE files.
      weeder.enable = lib.mkForce false;

      # The default werrorwolf override is `ghcOptions = [ "-Werror" ]`.
      # GHC 9.12 enables -Wx-partial by default, which turns existing `head`,
      # `tail`, `init`, `last`, `!!` call sites into errors under -Werror.
      # Disable the warning here (matches the justfile lint flag) so the
      # codebase can migrate off partial functions incrementally.
      werrorwolf.packageOverrideFunction = lib.mkForce (_exts: pkg: pkg.override { ghcOptions = [ "-Werror" "-Wno-x-partial" ]; });

    };
}
