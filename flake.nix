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
    hydra-spec.url = "github:cardano-scaling/hydra-formal-specification";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "haskellNix/nixpkgs";
    };
    mithril.url = "github:input-output-hk/mithril/2450.0";
    mithril-unstable.url = "github:input-output-hk/mithril/unstable";
    nixpkgs.follows = "haskellNix/nixpkgs";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = { self, ... }@inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.process-compose-flake.flakeModule
      ];
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      perSystem = { pkgs, config, lib, system, ... }:
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
              (final: prev: {
                aiken = inputs.aiken.packages.${system}.aiken;
                apply-refact = pkgs.haskell-nix.tool compiler "apply-refact" "0.15.0.0";
                cabal-fmt = pkgs.haskell-nix.tool compiler "cabal-fmt" "0.1.12";
                cabal-install = pkgs.haskell-nix.tool compiler "cabal-install" "3.10.3.0";
                cabal-plan = pkgs.haskell-nix.tool compiler "cabal-plan" "0.7.5.0";
                fourmolu = pkgs.haskell-nix.tool compiler "fourmolu" "0.17.0.0";
                haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" "2.9.0.0";
                hlint = pkgs.haskell-nix.tool compiler "hlint" "3.8";
                weeder = pkgs.haskell-nix.tool compiler "weeder" "2.9.0";
                cardano-cli = inputs.cardano-node.packages.${system}.cardano-cli;
                cardano-node = inputs.cardano-node.packages.${system}.cardano-node;
                mithril-client-cli = inputs.mithril.packages.${system}.mithril-client-cli;
                mithril-client-cli-unstable =
                  pkgs.writeShellScriptBin "mithril-client-unstable" ''
                    exec ${inputs.mithril-unstable.packages.${system}.mithril-client-cli}/bin/mithril-client "$@"
                  '';
              })
            ];
          };

          hydraPackageNames = [
            "hydra-cardano-api"
            "hydra-chain-observer"
            "hydra-cluster"
            "hydra-node"
            "hydra-tx"
            "hydra-prelude"
            "hydra-plutus"
            "hydra-plutus-extras"
            "hydra-test-utils"
            "hydra-tui"
            "hydraw"
          ];

          inputMap = { "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP; };

          hsPkgs = import ./nix/hydra/project.nix {
            inherit pkgs inputMap;
            compiler-nix-name = compiler;
          };

          hydraPackages = import ./nix/hydra/packages.nix {
            inherit pkgs inputs hsPkgs self;
            gitRev = self.rev or "dirty";
          };

          hydraImages = import ./nix/hydra/docker.nix {
            inherit hydraPackages pkgs;
          };

          prefixAttrs = s: attrs:
            with pkgs.lib.attrsets;
            mapAttrs' (name: value: nameValuePair (s + name) value) attrs;

          addWerror = x: x.override { ghcOptions = [ "-Werror" ]; };

          componentsToWerrors = n: x:
            builtins.listToAttrs
              ([
                {
                  name = "${n}-werror";
                  value = addWerror x.components.library;
                }
              ]) // lib.attrsets.mergeAttrsList (map
              (y:
                lib.mapAttrs'
                  (k: v: {
                    name = "${n}-${y}-${k}-werror";
                    value = addWerror v;
                  })
                  x.components."${y}") [ "benchmarks" "exes" "sublibs" "tests" ]);

          componentsToHieDirectories = x:
            [ x.components.library.hie ]
            ++ lib.concatLists
              (map
                (y:
                  lib.mapAttrsToList
                    (k: v:
                      v.hie
                    )
                    x.components."${y}") [ "benchmarks" "exes" "sublibs" "tests" ]);

          componentsToWeederArgs = x:
            builtins.concatStringsSep " " (map (z: "--hie-directory ${z}") (componentsToHieDirectories x));

          hydra-weeder = pkgs.runCommand "hydra-weeder" { buildInputs = [ pkgs.weeder ]; } ''
            mkdir -p $out
            weeder --config ${./weeder.toml} \
              ${builtins.concatStringsSep " " (map (x: componentsToWeederArgs hsPkgs."${x}") hydraPackageNames)}
          '';

          tx-cost-diff =
            let
              pyEnv = (pkgs.python3.withPackages (ps: with ps; [ pandas html5lib beautifulsoup4 tabulate ]));
            in
            pkgs.writers.writeHaskellBin
              "tx-cost-diff"
              {
                libraries =
                  with pkgs.haskellPackages;
                  [ aeson text bytestring lens lens-aeson shh ];
              } ''${builtins.readFile scripts/tx-cost-diff.hs}'';

        in
        {
          legacyPackages = pkgs // hsPkgs;

          packages =
            hydraPackages //
            (if pkgs.stdenv.isLinux then (prefixAttrs "docker-" hydraImages) else { }) // {
              spec = inputs.hydra-spec.packages.${system}.default;
              inherit tx-cost-diff;
            };
          process-compose."demo" = import ./nix/hydra/demo.nix {
            inherit system pkgs inputs self;
            inherit (pkgs) cardano-node cardano-cli process-compose;
            inherit (hydraPackages) hydra-node hydra-tui;
          };

          checks = let lu = inputs.lint-utils.linters.${system}; in {
            hlint = lu.hlint { src = self; hlint = pkgs.hlint; };
            treefmt = lu.treefmt {
              src = self;
              buildInputs = [
                pkgs.cabal-fmt
                pkgs.nixpkgs-fmt
                pkgs.fourmolu
              ];
              treefmt = pkgs.treefmt;
            };
            no-srp = lu.no-srp {
              src = self;
              cabal-project-file = ./cabal.project;
            };
            weeder = hydra-weeder;
          } // lib.attrsets.mergeAttrsList (map (x: componentsToWerrors x hsPkgs.${x}) hydraPackageNames);

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
