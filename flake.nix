{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    nixpkgsLatest.url = "github:NixOS/nixpkgs/nixos-24.05";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    hydra-spec.url = "github:cardano-scaling/hydra-formal-specification";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "haskellNix/nixpkgs";
    };
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    hls = {
      url = "github:haskell/haskell-language-server";
      flake = false;
    };
    cardano-node.url = "github:intersectmbo/cardano-node/9.2.0";
    mithril.url = "github:input-output-hk/mithril/2430.0";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
  };

  outputs =
    { self
    , flake-parts
    , nixpkgs
      # TODO remove when haskellNix updated to newer nixpkgs
    , nixpkgsLatest
    , cardano-node
    , ...
    } @ inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
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

          pkgsLatest = import nixpkgsLatest {
            inherit system;
          };
          pkgs = import nixpkgs {
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
                apply-refact = pkgs.haskell-nix.tool compiler "apply-refact" "0.14.0.0";
                cabal-fmt = pkgs.haskell-nix.tool compiler "cabal-fmt" "0.1.12";
                cabal-install = pkgs.haskell-nix.cabal-install.${compiler};
                cabal-plan = pkgs.haskell-nix.tool compiler "cabal-plan" "0.7.4.0";
                fourmolu = pkgs.haskell-nix.tool compiler "fourmolu" "0.16.2.0";
                haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" rec {
                  src = inputs.hls;
                  cabalProject = builtins.readFile (src + "/cabal.project");
                };
                hlint = pkgs.haskell-nix.tool compiler "hlint" "3.8";
                cardano-cli = inputs.cardano-node.packages.${system}.cardano-cli;
                cardano-node = inputs.cardano-node.packages.${system}.cardano-node;
                mithril-client-cli = inputs.mithril.packages.${system}.mithril-client-cli;
              })
            ];
          };

          inputMap = { "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP; };

          hsPkgs = import ./nix/hydra/project.nix {
            inherit pkgs inputMap;
            compiler-nix-name = compiler;
          };

          hydraPackages = import ./nix/hydra/packages.nix {
            inherit system pkgs inputs hsPkgs self;
            gitRev = self.rev or "dirty";
          };

          hydraImages = import ./nix/hydra/docker.nix {
            inherit hydraPackages system nixpkgs;
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
        in
        {
          legacyPackages = pkgs // hsPkgs;

          packages =
            hydraPackages //
            (if pkgs.stdenv.isLinux then (prefixAttrs "docker-" hydraImages) else { }) // {
              spec = inputs.hydra-spec.packages.${system}.default;
            };
          process-compose."demo" = import ./nix/hydra/demo.nix {
            inherit system pkgs inputs self;
            inherit (pkgsLatest) process-compose;
            inherit (pkgs) cardano-node cardano-cli;
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
              treefmt = pkgsLatest.treefmt;
            };
          } // lib.attrsets.mergeAttrsList (map (x: componentsToWerrors x hsPkgs.${x}) [
            "hydra-cardano-api"
            "hydra-chain-observer"
            "hydra-cluster"
            "hydra-explorer"
            "hydra-node"
            "hydra-tx"
            "hydra-plutus"
            "hydra-plutus-extras"
            "hydra-test-utils"
            "hydra-tui"
            "plutus-cbor"
            "plutus-merkle-tree"
          ]);

          devShells = import ./nix/hydra/shell.nix {
            inherit inputs pkgs hsPkgs system pkgsLatest;
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
