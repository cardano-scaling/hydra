{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "haskellNix/nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    # Use a patched 2.6.0.0 as we are also affected by
    # https://github.com/haskell/haskell-language-server/issues/4046
    hls = {
      url = "github:cardano-scaling/haskell-language-server?ref=2.6-patched";
      flake = false;
    };
    cardano-node.url = "github:intersectmbo/cardano-node/8.8.0-pre";
    mithril.url = "github:input-output-hk/mithril/2347.0";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
      (system:
      let
        compiler = "ghc964";

        # nixpkgs enhanced with haskell.nix and crypto libs as used by iohk

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
            # Reason: haskell.nix modules/overlays neds to be last
            # https://github.com/input-output-hk/haskell.nix/issues/1954
            inputs.haskellNix.overlay
            # Custom static libs used for darwin build
            (import ./nix/static-libs.nix)
            # Specific versions of tools we require
            (final: prev: {
              apply-refact = pkgs.haskell-nix.tool compiler "apply-refact" "0.14.0.0";
              cabal-fmt = pkgs.haskell-nix.tool compiler "cabal-fmt" "0.1.9";
              cabal-install = pkgs.haskell-nix.cabal-install.${compiler};
              cabal-plan = pkgs.haskell-nix.tool compiler "cabal-plan" "0.7.3.0";
              fourmolu = pkgs.haskell-nix.tool compiler "fourmolu" "0.14.0.0";
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
          inherit system pkgs inputs hsPkgs;
          gitRev = self.rev or "dirty";
        };

        hydraImages = import ./nix/hydra/docker.nix {
          inherit hydraPackages system nixpkgs;
        };

        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        legacyPackages = pkgs;

        packages =
          { default = hydraPackages.hydra-node; } //
          hydraPackages //
          prefixAttrs "docker-" hydraImages // {
            spec = import ./spec { inherit pkgs; };
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
        };

        devShells = import ./nix/hydra/shell.nix {
          inherit inputs pkgs hsPkgs system compiler;
        };

        # Build selected derivations in CI for caching
        hydraJobs = pkgs.lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
          packages = { inherit (packages) hydra-node hydra-tui hydraw spec; };
          devShells = { inherit (devShells) default; };
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
    ];
    allow-import-from-derivation = true;
  };
}
