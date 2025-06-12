{
  inputs = {
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    aiken.url = "github:aiken-lang/aiken/v1.1.9";
    cardano-node.url = "github:intersectmbo/cardano-node/10.1.4";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    hydra-coding-standards.url = "github:cardano-scaling/hydra-coding-standards/0.6.5";
    hydra-spec.url = "github:cardano-scaling/hydra-formal-specification/7f78e005b95ff3b9c55995632ceb3d6ab96a305e";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "haskellNix/nixpkgs";
    };
    mithril.url = "github:input-output-hk/mithril/2517.1";
    mithril-unstable.url = "github:input-output-hk/mithril/unstable";
    nixpkgs.follows = "haskellNix/nixpkgs";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = { self, ... }@inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.hydra-coding-standards.flakeModule
        inputs.process-compose-flake.flakeModule
        ./nix/hydra/demo.nix
        ./nix/hydra/docker.nix
      ];
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      perSystem = { config, lib, system, ... }:
        let
          compiler = "ghc966";

          # nixpkgs enhanced with haskell.nix and crypto libs as used by iohk

          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              # This overlay contains libsodium and libblst libraries
              inputs.iohk-nix.overlays.crypto
              # This overlay contains pkg-config mappings via haskell.nix to use the
              # crypto libraries above
              inputs.iohk-nix.overlays.haskell-nix-crypto
              # Keep haskell.nix as the last overlay!
              #
              # Reason: haskell.nix modules/overlays needs to be last
              # https://github.com/input-output-hk/haskell.nix/issues/1954
              inputs.haskellNix.overlay
              # Custom static libs used for darwin build
              (import ./nix/static-libs.nix)
              inputs.nix-npm-buildpackage.overlays.default
              # Specific versions of tools we require
              (final: _prev: {
                inherit (inputs.aiken.packages.${system}) aiken;
                apply-refact = final.haskell-nix.tool compiler "apply-refact" "0.15.0.0";
                cabal-install = final.haskell-nix.tool compiler "cabal-install" "3.10.3.0";
                cabal-plan = final.haskell-nix.tool compiler "cabal-plan" "0.7.5.0";
                cabal-fmt = config.treefmt.programs.cabal-fmt.package;
                fourmolu = config.treefmt.programs.fourmolu.package;
                haskell-language-server = final.haskell-nix.tool compiler "haskell-language-server" "2.9.0.0";
                weeder = final.haskell-nix.tool compiler "weeder" "2.9.0";
                inherit (inputs.cardano-node.packages.${system}) cardano-cli;
                inherit (inputs.cardano-node.packages.${system}) cardano-node;
                inherit (inputs.mithril.packages.${system}) mithril-client-cli;
                mithril-client-cli-unstable =
                  final.writeShellScriptBin "mithril-client-unstable" ''
                    exec ${inputs.mithril-unstable.packages.${system}.mithril-client-cli}/bin/mithril-client "$@"
                  '';
              })
            ];
          };

          inputMap = { "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP; };

          hsPkgs = import ./nix/hydra/project.nix {
            inherit pkgs inputMap;
            compiler-nix-name = compiler;
          };

          hydraPackages = import ./nix/hydra/packages.nix {
            inherit pkgs inputs hsPkgs self;
            gitRev = self.rev or "dirty";
          };

          tx-cost-diff =
            pkgs.writers.writeHaskellBin
              "tx-cost-diff"
              {
                libraries =
                  with pkgs.haskellPackages;
                  [ aeson text bytestring lens lens-aeson shh ];
              } ''${builtins.readFile scripts/tx-cost-diff.hs}'';

          allComponents = x:
            [ x.components.library ]
            ++ lib.concatMap
              (y: builtins.attrValues x.components."${y}")
              [ "benchmarks" "exes" "sublibs" "tests" ];
        in
        {

          _module.args = { inherit pkgs; };

          legacyPackages = pkgs // hsPkgs;

          packages =
            hydraPackages //
            {
              spec = inputs.hydra-spec.packages.${system}.default;
              inherit tx-cost-diff;
            };

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
            ];
            inherit (pkgs) weeder;
            haskellType = "haskell.nix";
            srp-check = false;
          };

          devShells = import ./nix/hydra/shell.nix {
            inherit pkgs hsPkgs hydraPackages system;
            ghc = pkgs.buildPackages.haskell-nix.compiler.${compiler};
          };
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
    ];
    allow-import-from-derivation = true;
  };
}
